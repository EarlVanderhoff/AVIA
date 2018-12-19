Option Strict On
Option Explicit On

Imports System.IO
Imports System.Threading

Public Structure EdgeVector
    Public Property Direction As Integer
    Public Property Magnitude As Integer
End Structure

Public Structure ImageVector
    Implements IComparable

    Public Property FrameNumber As Integer
    Public Property ImageVectorStandard As EdgeVector()
    Public Property ImageVectorEqualized As EdgeVector()

    Public Function CompareTo(ByVal obj As Object) As Integer Implements System.IComparable.CompareTo
        Return Me.FrameNumber.CompareTo(DirectCast(obj, ImageVector).FrameNumber)
    End Function
End Structure

Public Structure FinishedItem
    Public FrameNumber As Integer
    Public FileName As String
End Structure

Public Enum Side
    Master
    Compare
End Enum

Public Class EdgeDetect
    Implements IDisposable
    'Implements IEnumerable(Of EdgeDetectedImages)

#Region "Declarations"
    Private _disposing As Boolean
    Private _buffer As Integer
    Private _isProcessing As Boolean
    Private _processCounter As Integer = 1
    Private _currentFrameNumber As Integer = 1
    Private _waiting As Boolean
    Private _lastFrameNumber As Integer
    Private _listImageVectors As New List(Of ImageVector)

    Private _syncObjectGetItem As New Object
    Private _syncObjectAddItem As New Object

    ' Used to keep track of the number of threads in use
    Private _countdownEvent As CountdownEvent

    ' Used to signal the threadpool that a cancellation request has been made
    Private _cancellationToken As CancellationTokenSource

    Private Structure ProcessItem
        Friend FrameNumber As Integer
        Friend Filename As String
    End Structure

    Public Event EndProcessing(ByVal Done As Boolean)
    Public Event ProcessUpdate(ByVal Item As FinishedItem)
#End Region

#Region "Properties"
    Public Property Source As String
    Public Property SourceSide As Side
    Public Property CycleFrames As Integer
    Public Property ImageType As String
    Public Property ExcludePerimeter As Boolean = True

    Public ReadOnly Property IsProcessing As Boolean
        Get
            Return _isProcessing
        End Get
    End Property

    Public ReadOnly Property ImageVectors As List(Of ImageVector)
        Get
            Return _listImageVectors
        End Get
    End Property

    Public ReadOnly Property LastFrameNumber As Integer
        Get
            Return _lastFrameNumber
        End Get
    End Property
#End Region

#Region "Protected Methods"
    Protected Overridable Sub Dispose(ByVal Disposing As Boolean)
        If Not _disposing Then
            If Disposing Then
                ThreadsCleanup()
                If _countdownEvent IsNot Nothing Then _countdownEvent.Dispose()
            End If
        End If

        _disposing = True
    End Sub

    Protected Overrides Sub Finalize()
        Dispose(False)
        MyBase.Finalize()
    End Sub
#End Region

#Region "Private Methods"
    Private Sub ThreadsCleanup()
        ' implement this before release
    End Sub

    Private Function GetFilePath(ByVal sourceFolder As String, ByVal frameNumber As Integer) As String
        If AppTools.SessionLoaded Then
            If LCase(Me.SourceSide.ToString) = "master" Then
                Return Path.Combine(AppTools.ActiveMasterFolder, Me.SourceSide.ToString & "_" & frameNumber.ToString & Me.ImageType)
            Else : Return Path.Combine(AppTools.ActiveCompareFolder, Me.SourceSide.ToString & "_" & frameNumber.ToString & Me.ImageType)
            End If
        Else
            Dim folder As Integer = Convert.ToInt32(Conversion.Fix(frameNumber / Me.CycleFrames)) + 1
            Return Path.Combine(sourceFolder, folder.ToString, Me.SourceSide.ToString & "_" & frameNumber.ToString & Me.ImageType)
        End If

    End Function

    Private Function GetItem(ByVal frameNumber As Integer, ByRef list As List(Of ImageVector)) As ImageVector
        If frameNumber < list.Item(0).FrameNumber OrElse frameNumber > list.Item(list.Count).FrameNumber Then Return Nothing

        ' Check if the frame number is above or below the mean frame number in the list
        If frameNumber >= list.Item(Convert.ToInt32(list.Count / 2)).FrameNumber Then
            For index As Integer = Convert.ToInt32(list.Count / 2) To list.Count
                If frameNumber = list.Item(index).FrameNumber Then Return list.Item(index)
            Next
        Else
            For index As Integer = 0 To Convert.ToInt32(list.Count / 2)
                If frameNumber = list.Item(index).FrameNumber Then Return list.Item(index)
            Next
        End If

        Return Nothing
    End Function

    Private Sub ProcessImage(ByRef queue As List(Of ImageVector), ByVal frameNumber As Integer, ByVal fileName As String)
        If _cancellationToken.IsCancellationRequested Then Return

        Dim grayscaled As Bitmap = Nothing
        Dim Source As Bitmap = Nothing
        'Dim equalized As Bitmap = Nothing

        Try
            grayscaled = New Bitmap(fileName)
            Chroma.Remove(grayscaled)

            ' Apply median filter, and edge detection
            Source = DirectCast(grayscaled.Clone, Bitmap)
            Filter.ApplyMedianFilter(Source, Me.ExcludePerimeter)
            Dim vectorStandard As EdgeVector() = Sorbel.FastEdgeDetectNew(Source, 128)

            'Apply equalization, median filter, and edge detection
            Source = Sorbel.Equalize(grayscaled)
            Filter.ApplyMedianFilter(Source, Me.ExcludePerimeter)
            Dim vectorEqualized As EdgeVector() = Sorbel.FastEdgeDetectNew(Source, 128)

            ' Add the processed images to queue
            SyncLock _syncObjectAddItem
                _listImageVectors.Add(New ImageVector With {.FrameNumber = frameNumber,
                                                            .ImageVectorStandard = vectorStandard,
                                                            .ImageVectorEqualized = vectorEqualized})
                If frameNumber > _lastFrameNumber Then _lastFrameNumber = frameNumber

                ' Used by the consumer of the class
                RaiseEvent ProcessUpdate(New FinishedItem With {.FrameNumber = frameNumber, .FileName = fileName})
                _processCounter += 1
            End SyncLock
        Catch ex As Exception
            Throw
        Finally
            If Source IsNot Nothing Then Source.Dispose()
            'If equalized IsNot Nothing Then equalized.Dispose()
            If grayscaled IsNot Nothing Then grayscaled.Dispose()
        End Try
    End Sub

    Private Sub ProcessFile(ByVal Item As Object)
        If _cancellationToken.IsCancellationRequested Then Return

        Try
            ProcessImage(_listImageVectors, DirectCast(Item, ProcessItem).FrameNumber, DirectCast(Item, ProcessItem).Filename)
        Catch ex As Exception
            Throw
        Finally
            ' Decrement the CountDownEvent count
            _countdownEvent.Signal()

            If Not _waiting AndAlso _countdownEvent.CurrentCount = 0 Then
                ' Reset the CountdownEvent to its initial count
                _countdownEvent.Reset()

                ' CancellationTokenSource cannot be reused so we need to dispose it
                _cancellationToken.Dispose()

                ' Raise the EndProcessing event to signal the caller that the process has finished
                _isProcessing = False
                RaiseEvent EndProcessing(_isProcessing)

                _waiting = False
            End If
        End Try
    End Sub
#End Region

#Region "Public Methods"
    Public Sub New(ByVal buffer As Integer)
        _buffer = buffer
        _countdownEvent = New CountdownEvent(buffer + 1)
    End Sub

    Public Sub Dispose() Implements IDisposable.Dispose
        Dispose(True)
        GC.SuppressFinalize(Me)
    End Sub

    Public Function GetItem(ByVal index As Integer) As ImageVector
        SyncLock _syncObjectGetItem
            If _listImageVectors.Count = 0 Then
                Return Nothing
            Else
                Return _listImageVectors(index)
            End If
        End SyncLock
    End Function

    'Public Function GetItemByFrameNumber(ByVal frameNumber As Integer) As ImageVector
    '    SyncLock _syncObjectGetItem
    '        With _listImageVectors
    '            If .Count = 0 Then
    '                Return Nothing
    '            Else
    '                If frameNumber < .Item(0).FrameNumber OrElse frameNumber > .Item(.Count).FrameNumber Then Return Nothing

    '                ' Check if the frame number is above or below the mean frame number in the list
    '                If frameNumber >= .Item(Convert.ToInt32(.Count / 2)).FrameNumber Then
    '                    For index As Integer = Convert.ToInt32(.Count / 2) To .Count
    '                        If frameNumber = .Item(index).FrameNumber Then Return .Item(index)
    '                    Next
    '                Else
    '                    For index As Integer = 0 To Convert.ToInt32(.Count / 2)
    '                        If frameNumber = .Item(index).FrameNumber Then Return .Item(index)
    '                    Next
    '                End If
    '            End If
    '        End With
    '    End SyncLock
    'End Function

    ''' <summary>
    ''' 
    ''' </summary>
    ''' <param name="Retain">Retain the last number of items in the list</param>
    ''' <remarks></remarks>
    ''' 
    Public Sub ProcessFiles(ByVal Retain As Integer, ByVal startAt As Integer, ByVal wait As Boolean)
        If _isProcessing Then Return
        If Retain > _buffer Then Throw New ArgumentOutOfRangeException("Retain parameter must be lower than the set buffer")

        _isProcessing = True
        _processCounter = 1
        _waiting = wait

        If Retain = 0 Then
            _listImageVectors.Clear()
        ElseIf _listImageVectors.Count >= Retain Then
            _listImageVectors.RemoveRange(0, _listImageVectors.Count - Retain)
        End If

        _currentFrameNumber = startAt

        ' Create a CancellationTokenSource used to signal the threads that a cancellation is pending
        _cancellationToken = New CancellationTokenSource

        Dim top As Integer = _currentFrameNumber + _buffer
        For index As Integer = _currentFrameNumber To top
            If _cancellationToken.IsCancellationRequested Then Exit For

            Dim fileName As String = GetFilePath(Me.Source, index)
            'Trace.WriteLine("     " & fileName)

            If Not File.Exists(fileName) Then
                Trace.WriteLine("          does not exist!")

                ' Decrement the CountDownEvent count
                _countdownEvent.Signal()
                Continue For
            End If

            ThreadPool.QueueUserWorkItem(AddressOf ProcessFile, New ProcessItem With {.FrameNumber = index, .Filename = fileName})
            _currentFrameNumber = index
        Next

        If wait Then
            If Not _cancellationToken.IsCancellationRequested Then
                ' Wait for the threads to finish processing
                _countdownEvent.Wait(_cancellationToken.Token)
            End If

            ' Reset the CountdownEvent to its initial count
            _countdownEvent.Reset()

            ' CancellationTokenSource cannot be reused so we need to dispose it
            _cancellationToken.Dispose()

            ' Sort the list
            _listImageVectors.Sort()

            ' Raise the EndProcessing event to signal the caller that the process has finished
            _isProcessing = False
            RaiseEvent EndProcessing(_isProcessing)
        End If
    End Sub

    Public Sub ProcessFiles2(ByVal Retain As Integer, ByVal wait As Boolean, ByVal Scenes() As frmMain.SceneChanges)
        If _isProcessing Then Return
        If Retain > Scenes.Length Then Throw New ArgumentOutOfRangeException("Retain parameter must be lower than the set buffer")

        _isProcessing = True
        _processCounter = 1
        _waiting = wait
        _countdownEvent = New CountdownEvent(Scenes.Length)

        If Retain = 0 Then
            _listImageVectors.Clear()
        Else
            _listImageVectors.RemoveRange(0, _listImageVectors.Count - Retain)
        End If

        ' Create a CancellationTokenSource used to signal the threads that a cancellation is pending
        _cancellationToken = New CancellationTokenSource

        For index As Integer = 0 To Scenes.Length - 1
            If _cancellationToken.IsCancellationRequested Then Exit For

            Dim fileName As String = GetFilePath(Me.Source, Scenes(index).framenumber)

            If Not File.Exists(fileName) Then
                ' Decrement the CountDownEvent count
                _countdownEvent.Signal()
                Continue For
            End If

            ThreadPool.QueueUserWorkItem(AddressOf ProcessFile, New ProcessItem With {.FrameNumber = Scenes(index).framenumber, .Filename = fileName})
            _currentFrameNumber = index
        Next

        If wait Then
            If Not _cancellationToken.IsCancellationRequested Then
                ' Wait for the threads to finish processing
                _countdownEvent.Wait(_cancellationToken.Token)
            End If

            ' Reset the CountdownEvent to its initial count
            _countdownEvent.Reset()

            ' CancellationTokenSource cannot be reused so we need to dispose it
            _cancellationToken.Dispose()

            ' Sort the list
            _listImageVectors.Sort()

            ' Raise the EndProcessing event to signal the caller that the process has finished
            _isProcessing = False
            RaiseEvent EndProcessing(_isProcessing)
        End If
    End Sub
    Public Sub Cancel()
        If _isProcessing AndAlso _cancellationToken IsNot Nothing Then _cancellationToken.Cancel()
    End Sub
#End Region
End Class
