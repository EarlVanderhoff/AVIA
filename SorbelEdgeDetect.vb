Imports System.Drawing
Imports System.Drawing.Imaging
Imports System.Runtime.InteropServices

Module Sorbel
    Public Structure EdgeDetectedImages
        Public Property FrameNumber As Integer
        Public Property ImageVectorStd As EdgeVector()
        Public Property ImageVectorEQ As EdgeVector()
    End Structure
    Public TwoDimArr(281, 191) As Integer '(282, 192) As Integer
    Public ExaminedPixels(281, 191) As Integer '(282, 192) As Integer
    Public LeftVectorArray(0 To 351, 0 To 239) As EdgeVector
    Public RightVectorArray(0 To 351, 0 To 239) As EdgeVector
    Public CompareVectorArray(0 To 351, 0 To 239) As Integer
    'Public SorbelVectorArr(0 To 351, 0 To 239) As Integer
    'Dim SorbelGradientArr(0 To 351, 0 To 239)
    Public SaveLeftBlack_White As Bitmap
    Public SaveRightBlack_White As Bitmap
    'Public Function CannyEdgeDetect(ByVal Target As Integer, ByVal SourceBmp As Bitmap) As Bitmap
    '    'REMOVE CHROMA
    '    Dim BlackAndWhiteBmp As New Bitmap(RemoveChroma(SourceBmp))
    '    'RefreshPbox(Target, BlackAndWhiteBmp)
    '    'HISTOGRAM EQUALIZATION
    '    Dim EqualizedBmp As New Bitmap(Equalize(BlackAndWhiteBmp))
    '    'RefreshPbox(Target, EqualizedBmp)
    '    'FILTER
    '    Dim FilteredBmp As New Bitmap(MedianFilter(EqualizedBmp))
    '    'RefreshPbox(Target, FilteredBmp)
    '    'SORBEL EDGE DETECT
    '    Dim Threshold As Integer = 128
    '    Dim ImageBusiness As Decimal
    '    Dim SorbelBmp As New Bitmap(Sorbel.SorbelEdgeDetect(FilteredBmp, Threshold, ImageBusiness))
    '    'RefreshPbox(Target, SorbelBmp)
    '    'CANNY MAXIMUM SUPPRESSION AND HYSTERESIS
    '    Dim SuppressedBmp As New Bitmap(NonMaximumSuppression(SorbelBmp, Threshold))
    '    'RefreshPbox(Target, SuppressedBmp)
    '    'HYSTERESIS
    '    Dim Threshold2 As Integer = 30
    '    Dim HysteresisBmp As New Bitmap(AddHysteresis(SuppressedBmp, Threshold, Threshold2))
    '    'RefreshPbox(Target, HysteresisBmp)
    '    ''INVERT COLORS
    '    'Dim FinalBmp As New Bitmap(InvertColors(HysteresisBmp, Threshold2))
    '    'RefreshPbox(Target, FinalBmp)
    '    ''DETERMINE KPI
    '    'Dim HighlightBmp As New Bitmap(GetHighlights(Target, FinalBmp))
    '    'RefreshPbox(Target, HighlightBmp)

    '    Return HysteresisBmp 'FinalBmp
    'End Function
    'Private Sub RefreshPbox(target As Integer, bmp As Bitmap)
    '    If target = 0 Then
    '        frmMain.PictureBox1.Image = bmp
    '        frmMain.PictureBox1.Refresh()
    '    Else
    '        frmMain.PictureBox2.Image = bmp
    '        frmMain.PictureBox2.Refresh()
    '    End If
    'End Sub

    Public Function ErrorWeighting(ByVal PixArray() As Integer, Optional ByVal XZones As Boolean = False) As Integer
        'GOODBYE
        'DEVELOP 2-DIMENSIONAL ARRAY
        Dim PixWidth As Integer = 282
        Dim PixHeight As Integer = 192
        ReDim TwoDimArr(PixWidth - 1, PixHeight - 1)
        Dim FinalAnswer As Integer
        Dim LargestBlob As Integer
        Dim TotGray As Integer = 5000

        Try
            For intN As Integer = 0 To UBound(PixArray) - 1
                Dim intx As Integer = Math.Truncate(intN / PixHeight)
                Dim inty As Integer = intN - (intx * PixHeight)
                If XZones Then
                    With AppTools.ExcludeZone
                        If .FirstZone = True Then
                            If intx > .FirstLeft AndAlso intx < .FirstRight AndAlso inty > .FirstTop AndAlso inty < .FirstBottom Then
                                TwoDimArr(intx, inty) = 0
                                Continue For
                            End If
                        End If
                        If .SecondZone = True Then
                            If intx > .SecondLeft AndAlso intx < .SecondRight AndAlso inty > .SecondTop AndAlso inty < .SecondBottom Then
                                TwoDimArr(intx, inty) = 0
                                Continue For
                            End If
                        End If
                    End With
                End If
                Select Case PixArray(intN)
                    Case 1 'green
                        TwoDimArr(intx, inty) = 1
                    Case 2 ' red
                        TwoDimArr(intx, inty) = 2
                    Case 3 ' gray
                        TwoDimArr(intx, inty) = 3
                        TotGray += 1
                    Case 0 'white
                        TwoDimArr(intx, inty) = 0
                End Select
            Next
        Catch
            Dim i = 1
        Finally
            Dim i = 1
        End Try



        Dim ErrNo As Integer
        'CLEAR THE ERROR GRID
        ReDim ExaminedPixels(PixWidth - 1, PixHeight - 1) '(PixWidth, 0 To PixHeight)
        Try
            For intY As Integer = 0 To PixHeight - 2
                For intX As Integer = 0 To PixWidth - 2
                    'LOOK AT ALL UNEXAMINED HORIZONTAL PIXELS
                    If ExaminedPixels(intX, intY) = 0 Then
                        Dim intColor As Integer = TwoDimArr(intX, intY)
                        '0 = white, 1 = green, 2 = red, 3 = gray
                        If intColor = 0 Or intColor = 3 Then
                            ExaminedPixels(intX, intY) = 10000
                            Continue For
                        Else 'FIRST RED OR GREEN FOUND - VERIFY BOX
                            Dim LastBox As Integer = 0
                            Dim FirstBox As Integer = 0
                            Dim TotalBoxes As Integer = 0
                            For NextIntX As Integer = intX To PixWidth - 2
                                If BoxVerifiedFromArrays(intColor, NextIntX, intY, ErrNo, False) Then
                                    If FirstBox = 0 Then FirstBox = intX
                                    LastBox = NextIntX
                                    TotalBoxes += 1
                                Else : Exit For
                                End If
                            Next

                            'OK - I'VE FOUND A RED OR GREEN PIXEL, VERIFIED THAT IT IS PART OF A LARGER FILED (BOX)
                            ' AND DETERMINED THE HORIZONTAL (L-R) BOUNDARIES OF THAT FIELD (FIRSTBOX, LASTBOX)

                            If LastBox = 0 Then Continue For 'EXIT AND CONTINUE WITHOUT WEIGHTING IF NO BOX FOUND

                            Dim EarliestBoxOnAnyRow As Integer = FirstBox
                            Dim LatestBoxOnAnyRow As Integer = LastBox
                            Dim FirstRow As Integer = intY
                            Dim LastRow As Integer = intY

                            'EXAMINE THE AREA UNDER EACH FOUND BOX FOR THE EXISTENCE OF ANOTHER BOX
                            For NextIntY As Integer = intY + 1 To PixHeight - 2
                                Dim NewBoxesInRow As Integer = 0
                                'EXAMINE THE AREA UNDER EACH FOUND BOX FOR THE EXISTENCE OF ANOTHER BOX
                                Dim NextFirstBox As Integer = 0
                                Dim NextLastBox As Integer = 0
                                For NextRowIntX As Integer = FirstBox To LastBox
                                    If ExaminedPixels(NextRowIntX, NextIntY) <> ErrNo Then Continue For
                                    If BoxVerifiedFromArrays(intColor, NextRowIntX, NextIntY, ErrNo, False) Then
                                        If NextFirstBox = 0 Then NextFirstBox = NextRowIntX
                                        NextLastBox = NextRowIntX
                                        NewBoxesInRow += 1
                                    End If
                                Next

                                'CHECK AREA TO THE LEFT OF THIS ROW'S EARLIEST BOX - RESET EARLIEST BOX IF NECESSARY
                                If NextFirstBox = FirstBox Then
                                    For NextRowIntX As Integer = NextFirstBox To 1 Step -1
                                        If BoxVerifiedFromArrays(intColor, NextRowIntX, NextIntY, ErrNo, False) Then
                                            If NextRowIntX < EarliestBoxOnAnyRow Then EarliestBoxOnAnyRow = NextRowIntX
                                            NextFirstBox = NextRowIntX
                                            NewBoxesInRow += 1
                                        Else : Exit For
                                        End If
                                    Next
                                End If
                                'CHECK THE AREA TO THE RIGHT OF THIS ROW'S FINAL BOX - RESET FINAL BOX IF NECESSARY
                                If NextLastBox = LastBox Then
                                    For NextRowIntX As Integer = LastBox To PixWidth - 2
                                        If BoxVerifiedFromArrays(intColor, NextRowIntX, NextIntY, ErrNo, False) Then
                                            If NextRowIntX > LatestBoxOnAnyRow Then LatestBoxOnAnyRow = NextRowIntX
                                            NextLastBox = NextRowIntX
                                            NewBoxesInRow += 1
                                        Else : Exit For
                                        End If
                                    Next
                                End If

                                If NewBoxesInRow = 0 Then Exit For
                                TotalBoxes += NewBoxesInRow
                                LastRow = NextIntY
                                FirstBox = NextFirstBox
                                LastBox = NextLastBox
                            Next

                            'GET THE AVERAGE HEIGHT AND WIDTH THEN WEIGHT THE ERROR
                            Dim AbsoluteWidth As Integer = LatestBoxOnAnyRow - EarliestBoxOnAnyRow
                            Dim AbsoluteHeight As Integer = LastRow - FirstRow
                            If AbsoluteHeight > 0 And AbsoluteWidth > 0 Then
                                Dim AvgWidth As Decimal = TotalBoxes / AbsoluteWidth
                                Dim AvgHeight As Decimal = TotalBoxes / AbsoluteHeight
                                Dim WidthWeight As Decimal = Math.Min((AvgWidth * AvgWidth), 100)
                                Dim heightweight As Decimal = Math.Min((AvgHeight * AvgHeight), 100)

                                Dim MinimumDimension As Decimal = Math.Min(WidthWeight, heightweight)
                                Dim ThisBlob As Decimal = TotalBoxes * MinimumDimension
                                LargestBlob = Math.Max(LargestBlob, ThisBlob)
                                FinalAnswer += ThisBlob
                            End If
                        End If
                    End If
                Next
            Next
            'FinalAnswer = ((FinalAnswer * 2000 / TotGray) + 2 * LargestBlob) / 3
            FinalAnswer = ((FinalAnswer * 2500 / TotGray) + 2 * LargestBlob) / 3
            Return FinalAnswer
        Catch ex As Exception
            Trace.WriteLine("sorbel.errorweighting -> " & ex.Message)
            Return Nothing
        Finally

        End Try

    End Function
    Public Function BoxVerifiedFromArrays(ByVal ColorInteger As Integer, ByVal XCoord As Integer, ByVal YCoord As Integer, ByRef ErrNo As Integer, ByVal KeepErr As Boolean) As Boolean
        'VERIFIES THE PIXEL BOX IS HOMOGENOUS AND OF THE CORRECT ERROR COLOR
        'IF TRUE IT ALSO SETS ALL PIXEL VALUES TO EXAMINED
        Dim RightPixel As Integer = TwoDimArr(XCoord + 1, YCoord)
        Dim DownPixel As Integer = TwoDimArr(XCoord, YCoord + 1)
        Dim Right_DownPixel As Integer = TwoDimArr(XCoord + 1, YCoord + 1)

        If RightPixel = ColorInteger AndAlso DownPixel = ColorInteger AndAlso Right_DownPixel = ColorInteger Then
            If Not KeepErr Then ErrNo += 1
            ExaminedPixels(XCoord, YCoord) = ErrNo
            ExaminedPixels(XCoord, YCoord + 1) = ErrNo
            ExaminedPixels(XCoord + 1, YCoord) = ErrNo
            ExaminedPixels(XCoord + 1, YCoord + 1) = ErrNo
            Return True
        End If

        Return False
    End Function
    Public Function JustifyImagesQualified(ByVal AsInput() As Integer, ByVal Equalized() As Integer) As Integer()
        Dim Fin(UBound(AsInput)) As Integer
        Try

            For intN As Integer = 0 To UBound(AsInput) - 1

                If Equalized(intN) = 0 Then ' equalized is white
                    Fin(intN) = AsInput(intN)    'set the final array to equal the as input array
                ElseIf AsInput(intN) = 1 Then 'as input is green
                    If Equalized(intN) <> 1 Then 'equalized is not green 
                        Fin(intN) = 0 ' set the final array to equal white
                    Else 'equalized is green
                        Fin(intN) = 1 ' set the final array to equal green - error is continued from equalized version
                    End If
                ElseIf AsInput(intN) = 2 Then 'as input is red
                    If Equalized(intN) <> 2 Then ' equalized is not red
                        Fin(intN) = 0 ' set the final array to equal white
                    Else 'equalized is red
                        Fin(intN) = 2 ' set the final array to equal red - error is continued from equalized version
                    End If
                ElseIf AsInput(intN) = 3 OrElse AsInput(intN) = 0 Then 'as input side is gray or white
                    Fin(intN) = AsInput(intN)
                End If
            Next

        Catch ex As Exception
            Trace.WriteLine("sorbel.justifyimagesqualified -> " & ex.Message)
            Return AsInput
        End Try
        Return Fin

    End Function

    Public Function RemoveChroma(ByRef SourceBitmap As Bitmap) As Bitmap
        Dim original As Bitmap = DirectCast(SourceBitmap.Clone, Bitmap)
        Dim colorMatrix As New ColorMatrix(New Single()() _
                                                {New Single() {0.3, 0.3, 0.3, 0, 0}, _
                                                 New Single() {0.59, 0.59, 0.59, 0, 0}, _
                                                 New Single() {0.11, 0.11, 0.11, 0, 0}, _
                                                 New Single() {0, 0, 0, 1, 0}, _
                                                 New Single() {0, 0, 0, 0, 1}}) 'create the grayscale ColorMatrix

        Try
            Using objGraphics As Graphics = Graphics.FromImage(SourceBitmap) 'get a graphics object from the new image 
                Using iattributesGrayscale As New ImageAttributes() 'create some image attributes 
                    iattributesGrayscale.SetColorMatrix(colorMatrix) 'set the color matrix attribute 
                    objGraphics.DrawImage(original, New Rectangle(0, 0, original.Width, original.Height), 0, 0, original.Width, original.Height, GraphicsUnit.Pixel, iattributesGrayscale) 'draw the original image on the new image using the grayscale color matrix 
                End Using
            End Using

            Return original
        Catch ex As Exception
            Throw
        Finally
            If original IsNot Nothing Then original.Dispose()
        End Try
    End Function

    Public Function Equalize(ByVal Bmp As Bitmap) As Bitmap
        Dim workingBitmap As Bitmap = Bmp
        Dim dataBitmap As BitmapData = Nothing

        Try
            ' Copy all pixels to array
            dataBitmap = workingBitmap.LockBits(New Rectangle(New Point, workingBitmap.Size), ImageLockMode.ReadWrite, PixelFormat.Format24bppRgb)
            Dim pixels(Math.Abs(dataBitmap.Stride) * dataBitmap.Height) As Byte
            Marshal.Copy(dataBitmap.Scan0, pixels, 0, pixels.Length)

            ' Populate the histogram
            Dim histogram(255) As Integer
            For x As Integer = 0 To workingBitmap.Width - 1
                For y As Integer = 0 To workingBitmap.Height - 1
                    Dim offset As Integer = (dataBitmap.Stride * y) + (x * 3)
                    histogram(pixels(offset + 2)) += 1
                Next
            Next

            ' Get the cummulative frequency values
            ' Not sure if this can still be optimized...
            Dim CDFArray(255) As Integer
            For index As Integer = 0 To 255
                For prior As Integer = index To 0 Step -1
                    CDFArray(index) += histogram(prior)
                Next
            Next

            ' Just get all non-zero items
            Dim minimum As Integer = 1000000
            For intn As Integer = 0 To 255
                If CDFArray(intn) <> 0 Then minimum = Math.Min(minimum, CDFArray(intn))
            Next

            ' Equalize the bitmap by applying the cdf values
            If minimum < (workingBitmap.Width * workingBitmap.Height) Then
                For x As Integer = 0 To workingBitmap.Width - 1
                    For y As Integer = 0 To workingBitmap.Height - 1
                        Dim offset As Integer = (dataBitmap.Stride * y) + (x * 3)
                        Dim value As Integer = CDFArray(pixels(offset + 2))
                        Dim equalizedShade As Byte = Convert.ToByte((255 * (value - minimum)) / ((workingBitmap.Width * workingBitmap.Height) - minimum))

                        ' Red, Green, Blue
                        pixels(offset + 2) = equalizedShade
                        pixels(offset + 1) = equalizedShade
                        pixels(offset) = equalizedShade
                    Next
                Next

                ' Apply the new equalized pixels back to the original bitmap
                Marshal.Copy(pixels, 0, dataBitmap.Scan0, pixels.Length)
                'workingBitmap.UnlockBits(dataBitmap)
            End If
            workingBitmap.UnlockBits(dataBitmap)
            Return workingBitmap.Clone
        Catch ex As Exception
            Throw
        Finally
            If Not workingBitmap Is Nothing Then workingBitmap.Dispose()
            If Not dataBitmap Is Nothing Then dataBitmap = Nothing
        End Try
    End Function

    Public Function MedianFilter(ByVal bmp As Bitmap) As Bitmap
        'APPLY THE MEDIAN FILTER
        Dim FilterBmp As New Bitmap(bmp.Width, bmp.Height)
        Dim lstLum As New ArrayList
        For intX As Integer = 0 To bmp.Width - 1
            For inty As Integer = 0 To bmp.Height - 1
                Dim thispixel As Color
                Dim MedianLum As Integer
                If intX < 1 Or intX > bmp.Width - 2 Or inty < 1 Or inty > bmp.Height - 2 Then
                    MedianLum = 255
                Else
                    For intRow As Integer = -1 To 1
                        For intCol As Integer = -1 To 1
                            thispixel = bmp.GetPixel(intX + intRow, inty + intCol)
                            lstLum.Add(thispixel.R)
                        Next
                    Next
                    lstLum.Sort()
                    MedianLum = lstLum(4)
                    'If Not MedianLum = 0 AndAlso Not MedianLum = 255 Then Trace.WriteLine(intX.ToString & ":" & inty.ToString & " = " & MedianLum)
                    lstLum.Clear()
                End If
                FilterBmp.SetPixel(intX, inty, Color.FromArgb(MedianLum, MedianLum, MedianLum))
            Next
        Next

        Return FilterBmp
    End Function

    'Public Function FastMedianFilter(ByVal bmp As Bitmap) As Bitmap
    '    'APPLY THE MEDIAN FILTER
    '    Dim FilterBmp As New Bitmap(bmp.Width, bmp.Height)
    '    Dim lstLum As New ArrayList
    '    For intX As Integer = 0 To bmp.Width - 1
    '        For inty As Integer = 0 To bmp.Height - 1
    '            Dim thispixel As Color
    '            Dim MedianLum As Integer
    '            If intX < 1 Or intX > bmp.Width - 2 Or inty < 1 Or inty > bmp.Height - 2 Then
    '                MedianLum = 255
    '            Else
    '                For intRow As Integer = -1 To 1
    '                    For intCol As Integer = -1 To 1
    '                        thispixel = bmp.GetPixel(intX + intRow, inty + intCol)
    '                        lstLum.Add(thispixel.R)
    '                    Next
    '                Next
    '                lstLum.Sort()
    '                MedianLum = lstLum(4)
    '                lstLum.Clear()
    '            End If
    '            FilterBmp.SetPixel(intX, inty, Color.FromArgb(MedianLum, MedianLum, MedianLum))
    '        Next
    '    Next

    '    Return FilterBmp
    'End Function

    Public Function FastMedianFilter(ByVal bmp As Bitmap) As Bitmap
        Dim dataBmp As BitmapData
        dataBmp = bmp.LockBits(New Rectangle(New Point, bmp.Size), ImageLockMode.ReadOnly, PixelFormat.Format24bppRgb)
        Dim pointerBmp As IntPtr = dataBmp.Scan0

        Dim FilteredBmp As New Bitmap(bmp.Width, bmp.Height)
        Dim dataFiltered As BitmapData
        dataFiltered = FilteredBmp.LockBits(New Rectangle(New Point, FilteredBmp.Size), ImageLockMode.WriteOnly, PixelFormat.Format24bppRgb)
        Dim PointerFilter As IntPtr = dataFiltered.Scan0

        'APPLY THE MEDIAN FILTER
        Dim lstLum As New ArrayList
        For intX As Integer = 0 To bmp.Width - 1
            For inty As Integer = 0 To bmp.Height - 1
                Dim intOffset As Integer = dataBmp.Stride * inty + (3 * intX)
                'Dim thispixel As Color
                Dim ThisGrayshade As Byte
                Dim MedianLum As Byte
                If intX < 1 Or intX > bmp.Width - 2 Or inty < 1 Or inty > bmp.Height - 2 Then
                    'MedianLum = 255
                    Marshal.WriteByte(PointerFilter, intOffset, 255)
                    Marshal.WriteByte(PointerFilter, intOffset + 1, 255)
                    Marshal.WriteByte(PointerFilter, intOffset + 2, 255)
                Else
                    For intRow As Integer = -1 To 1
                        For intCol As Integer = -1 To 1
                            ThisGrayshade = Marshal.ReadByte(pointerBmp, intOffset)
                            lstLum.Add(ThisGrayshade)
                            'thispixel = bmp.GetPixel(intX + intRow, inty + intCol)
                            'lstLum.Add(thispixel.R)
                        Next
                    Next
                    lstLum.Sort()
                    MedianLum = lstLum(4)
                    lstLum.Clear()
                End If
                Marshal.WriteByte(PointerFilter, intOffset, MedianLum)
                Marshal.WriteByte(PointerFilter, intOffset + 1, MedianLum)
                Marshal.WriteByte(PointerFilter, intOffset + 2, MedianLum)
                'FilterBmp.SetPixel(intX, inty, Color.FromArgb(MedianLum, MedianLum, MedianLum))
            Next
        Next

        bmp.UnlockBits(dataBmp)
        FilteredBmp.UnlockBits(dataFiltered)

        If bmp IsNot Nothing Then bmp.Dispose()

        Return FilteredBmp
    End Function

    Public Function FramingDelta(ByVal LeftBmp As Bitmap, ByVal RightBmp As Bitmap, ByRef strReturn As String) As Bitmap
        Dim TotDelta As Integer
        Dim GrayShade As Integer
        Dim intTenPercentWidth As Integer = Convert.ToInt32((LeftBmp.Width - 1) / 10)
        Dim intTenPercentHeight As Integer = Convert.ToInt32((LeftBmp.Height - 1) / 10)

        Dim rectangleBounds As New Rectangle(0, 0, LeftBmp.Width, LeftBmp.Height)

        Dim dataLeftBitmap As BitmapData
        dataLeftBitmap = LeftBmp.LockBits(rectangleBounds, ImageLockMode.ReadOnly, PixelFormat.Format24bppRgb)
        Dim PointerLeft As IntPtr = dataLeftBitmap.Scan0

        Dim dataRightBitmap As BitmapData
        dataRightBitmap = RightBmp.LockBits(rectangleBounds, ImageLockMode.ReadOnly, PixelFormat.Format24bppRgb)
        Dim PointerRight As IntPtr = dataRightBitmap.Scan0

        Dim ReturnBmp As New Bitmap(LeftBmp.Width, LeftBmp.Height)
        Dim dataReturnBitmap As BitmapData
        dataReturnBitmap = ReturnBmp.LockBits(rectangleBounds, ImageLockMode.WriteOnly, PixelFormat.Format24bppRgb)
        Dim PointerReturn As IntPtr = dataReturnBitmap.Scan0

        For intX As Integer = 0 To LeftBmp.Width - 1
            For intY As Integer = 0 To LeftBmp.Height - 1
                Dim intOffset As Integer = dataReturnBitmap.Stride * intY + (3 * intX)
                If intX < intTenPercentWidth OrElse intX > LeftBmp.Width - intTenPercentWidth OrElse intY < intTenPercentHeight OrElse intY > LeftBmp.Height - intTenPercentHeight Then
                    Marshal.WriteByte(PointerReturn, intOffset, 255)
                    Marshal.WriteByte(PointerReturn, intOffset + 1, 255)
                    Marshal.WriteByte(PointerReturn, intOffset + 2, 255)
                    Continue For
                End If

                Dim Left As Boolean = False
                If Marshal.ReadByte(PointerLeft, intOffset) = 0 Then Left = True

                Dim Right As Boolean = False
                If Marshal.ReadByte(PointerRight, intOffset) = 0 Then Right = True

                If Left AndAlso Not Right Then
                    Marshal.WriteByte(PointerReturn, intOffset, 0)
                    Marshal.WriteByte(PointerReturn, intOffset + 1, 128)
                    Marshal.WriteByte(PointerReturn, intOffset + 2, 0)
                ElseIf Right AndAlso Not Left Then
                    Marshal.WriteByte(PointerReturn, intOffset, 0)
                    Marshal.WriteByte(PointerReturn, intOffset + 1, 0)
                    Marshal.WriteByte(PointerReturn, intOffset + 2, 128)
                ElseIf Right AndAlso Left Then
                    Marshal.WriteByte(PointerReturn, intOffset, 190)
                    Marshal.WriteByte(PointerReturn, intOffset + 1, 190)
                    Marshal.WriteByte(PointerReturn, intOffset + 2, 190)
                    GrayShade += 1
                Else
                    Marshal.WriteByte(PointerReturn, intOffset, 255)
                    Marshal.WriteByte(PointerReturn, intOffset + 1, 255)
                    Marshal.WriteByte(PointerReturn, intOffset + 2, 255)
                End If

                Dim ThisDelta As Integer = Math.Abs(CInt(Left) - CInt(Right))
                TotDelta += ThisDelta
            Next
        Next
        Win32APIs.DeleteObject(PointerLeft)
        Win32APIs.DeleteObject(PointerRight)
        Win32APIs.DeleteObject(PointerReturn)

        LeftBmp.UnlockBits(dataLeftBitmap)
        RightBmp.UnlockBits(dataRightBitmap)
        ReturnBmp.UnlockBits(dataReturnBitmap)

        strReturn = TotDelta.ToString & " / " & GrayShade.ToString
        Return ReturnBmp
    End Function

    Public Function BuildComposite(ByVal LeftVelocityArr() As EdgeVector, ByVal RightVelocityArr() As EdgeVector) As Integer()

        Dim ArrWidth As Integer = 282
        Dim UpperBound As Integer = UBound(LeftVelocityArr)
        Dim ReturnArr(UpperBound) As Integer

        For intN As Integer = 0 To UpperBound
            Dim Left As Boolean = False
            Dim Right As Boolean = False
            Dim LeftPointVelocity As EdgeVector = LeftVelocityArr(intN)
            If LeftPointVelocity.Direction <> 180 Then Left = True
            Dim RightPointVelocity As EdgeVector = RightVelocityArr(intN)
            If RightPointVelocity.Direction <> 180 Then Right = True
            If Left AndAlso Not Right Then 'green
                ReturnArr(intN) = 1
            ElseIf Right AndAlso Not Left Then 'red
                ReturnArr(intN) = 2
            ElseIf Right AndAlso Left Then 'gray
                ReturnArr(intN) = 3
            Else 'white
                ReturnArr(intN) = 0
            End If
        Next

        Return ReturnArr
    End Function

    Public Function JustifyImages(ByVal AsInput() As Integer, ByVal Equalized() As Integer) As Integer()
        Dim Fin(UBound(AsInput)) As Integer

        For intN As Integer = 0 To UBound(AsInput) - 1

            If Equalized(intN) = 0 Then ' equalized is white
                Fin(intN) = AsInput(intN)    'set the final array to equal the as input array
            ElseIf AsInput(intN) = 1 Then 'as input is green
                If Equalized(intN) <> 1 Then 'equalized is not green 
                    Fin(intN) = 3 ' set the final array to equal gray
                Else 'equalized is green
                    Fin(intN) = 1 ' set the final array to equal green - error is continued in equalized version
                End If
            ElseIf AsInput(intN) = 2 Then 'as input is red
                If Equalized(intN) <> 2 Then ' equalized is not red
                    Fin(intN) = 3 ' set the final array to equal gray
                Else 'equalized is red
                    Fin(intN) = 2 ' set the final array to equal red - error is continued in equalized version
                End If
            ElseIf AsInput(intN) = 3 OrElse Equalized(intN) = 3 Then 'either side is gray
                Fin(intN) = 3
            Else 'as input is white
                Fin(intN) = 0
            End If

        Next

        Return Fin

    End Function

    Public Function RemoveHorizontalAdjacents(ByVal JustArr() As Integer, LeftVelocityArr() As EdgeVector, RightVelocityArr() As EdgeVector) As Integer()
        Dim FinArr(UBound(JustArr)) As Integer
        Dim PixHeight As Integer = 192
        Dim PixWidth As Integer = 282

        For intN As Integer = 0 To UBound(FinArr) - 1
            'IF COLOR IS GREEN OR RED, THEN GET DIRECTION FROM THE APPROPRIATE VECTOR ARRAY
            Dim ReDrawGray As Boolean = False
            Dim ThisDirection As Integer
            Dim AlternateColor As Integer
            'IF COLOR IS GREEN OR RED, THEN GET DIRECTION FROM THE APPROPRIATE VECTOR ARRAY
            If JustArr(intN) = 1 Or JustArr(intN) = 2 Then
                If JustArr(intN) = 1 Then
                    ThisDirection = LeftVelocityArr(intN).Direction
                    AlternateColor = 2
                ElseIf JustArr(intN) = 2 Then
                    ThisDirection = RightVelocityArr(intN).Direction
                    AlternateColor = 1
                End If

                'NOT HORIZONTAL OR DIAGONAL - IGNORE IT AND MOVE ON
                If ThisDirection <> 90 Then '
                    FinArr(intN) = JustArr(intN)
                    Continue For
                End If

                'OK - HERE I'VE LOCATED A RED OR GREEN PIXEL WHOSE VELOCITY IS NOT VERTICAL
                'FIND BOTTOM
                Dim inc As Integer = 0
                Dim intBottomPixel As Integer
                Dim WasWhiteOrUniqDirection As Boolean = False
                Dim BottomBorder As Boolean = False
                Do
                    'NEXT PIXEL DOWN
                    inc += PixWidth
                    intBottomPixel = intN + inc
                    If intBottomPixel > UBound(JustArr) Then
                        intBottomPixel -= PixWidth
                        Exit Do
                    End If


                    'PIXEL IS WHITE
                    If JustArr(intBottomPixel) = 0 Then
                        'ALLOW ONE WHITE LINE BETWEEN EDGES WITHOUT DECLARING ERROR
                        If Not WasWhiteOrUniqDirection Then
                            WasWhiteOrUniqDirection = True
                        Else
                            Exit Do
                        End If

                        'PIXEL IS ALTERNATE COLOR OR GRAY
                    ElseIf JustArr(intBottomPixel) = AlternateColor OrElse JustArr(intBottomPixel) = 3 Then
                        If RightVelocityArr(intBottomPixel).Direction <> LeftVelocityArr(intBottomPixel).Direction Then
                            If Not WasWhiteOrUniqDirection Then
                                WasWhiteOrUniqDirection = True
                            Else
                                Exit Do
                            End If
                        End If
                        'CONTINUE DOWNWARD - MARK THE AREA FOR RECOLORATION TO GRAY
                        BottomBorder = True
                        intBottomPixel -= PixWidth
                        ReDrawGray = True
                        Exit Do

                        'PIXEL IS THE SAME COLOR
                    Else
                        'DONT RETURN TO THE SAME COLOR IF A WHITE BREAK HAS BEEN DETECTED
                        If WasWhiteOrUniqDirection Then Exit Do
                    End If
                Loop

                'FIND TOP
                inc = 0
                Dim TopBorder As Boolean = False
                Dim intTopPixel As Integer
                Do
                    inc -= PixWidth
                    intTopPixel = intN + inc
                    If intTopPixel < 0 Then
                        intTopPixel += PixWidth
                        Exit Do
                    End If

                    'WHITE
                    If JustArr(intTopPixel) = 0 Then
                        If Not WasWhiteOrUniqDirection Then
                            WasWhiteOrUniqDirection = True
                        Else
                            Exit Do
                        End If

                        'ALTERNATE COLOR OR GRAY
                    ElseIf JustArr(intTopPixel) = AlternateColor OrElse JustArr(intTopPixel) = 3 Then
                        If RightVelocityArr(intTopPixel).Direction <> LeftVelocityArr(intTopPixel).Direction Then
                            If Not WasWhiteOrUniqDirection Then
                                WasWhiteOrUniqDirection = True
                            Else
                                Exit Do
                            End If
                        End If
                        TopBorder = True
                        intTopPixel += PixWidth
                        ReDrawGray = True
                        Exit Do

                        'SAME COLOR
                    Else
                        If WasWhiteOrUniqDirection Then Exit Do
                    End If
                Loop

                If ReDrawGray Then
                    For NewGrayPixel As Integer = intTopPixel To intBottomPixel Step PixWidth
                        FinArr(NewGrayPixel) = 3
                        AppTools.GrayRemoved += 1
                    Next
                End If

            End If
            If Not ReDrawGray Then
                FinArr(intN) = JustArr(intN)
            End If
        Next

        Return FinArr

    End Function
    Public Function Remove45DegreeAdjacents(ByVal JustArr() As Integer, LeftVelocityArr() As EdgeVector, RightVelocityArr() As EdgeVector) As Integer()
        Dim FinArr(UBound(JustArr)) As Integer
        Dim PixHeight As Integer = 192
        Dim PixWidth As Integer = 282
        Dim MaxPix As Integer = PixHeight * PixWidth - 1


        For intN As Integer = 0 To UBound(FinArr) - 1
            'IF COLOR IS GREEN OR RED, THEN GET DIRECTION FROM THE APPROPRIATE VECTOR ARRAY
            Dim ReDrawGray As Boolean = False
            Dim ThisDirection As Integer
            Dim AlternateColor As Integer
            If JustArr(intN) = 2 Or JustArr(intN) = 1 Then
                If JustArr(intN) = 1 Then
                    ThisDirection = LeftVelocityArr(intN).Direction
                    AlternateColor = 2
                ElseIf JustArr(intN) = 2 Then
                    ThisDirection = RightVelocityArr(intN).Direction
                    AlternateColor = 1
                End If

                'NOT VERTICAL DIAGONAL - IGNORE IT AND MOVE ON
                If ThisDirection <> 45 Then
                    FinArr(intN) = JustArr(intN)
                    Continue For
                End If

                'OK - HERE I'VE LOCATED A RED OR GREEN PIXEL WHOSE VELOCITY IS NOT VERTICAL
                'FIND RIGHT
                Dim inc As Integer = 0
                Dim intRightPixel As Integer
                Dim WasWhiteOrUniqDirection As Boolean = False
                Do
                    'NEXT PIXEL RIGHT
                    inc -= (PixWidth - 1)
                    intRightPixel = intN + inc
                    If intRightPixel < 0 Then '(intRightPixel + 1) Mod PixWidth = 0 Then
                        intRightPixel += (PixWidth - 1)
                        Exit Do
                    End If

                    'PIXEL IS WHITE
                    If JustArr(intRightPixel) = 0 Then
                        'ALLOW ONE WHITE LINE BETWEEN EDGES WITHOUT DECLARING ERROR
                        If Not WasWhiteOrUniqDirection Then
                            WasWhiteOrUniqDirection = True
                        Else
                            Exit Do
                        End If

                        'PIXEL IS ALTERNATE COLOR OR GRAY
                    ElseIf JustArr(intRightPixel) = AlternateColor OrElse JustArr(intRightPixel) = 3 Then
                        If RightVelocityArr(intRightPixel).Direction <> LeftVelocityArr(intRightPixel).Direction Then
                            If Not WasWhiteOrUniqDirection Then
                                WasWhiteOrUniqDirection = True
                            Else
                                Exit Do
                            End If
                        End If
                        intRightPixel += (PixWidth - 1)
                        ReDrawGray = True
                        Exit Do

                        'SAME COLOR
                    Else
                        If WasWhiteOrUniqDirection Then Exit Do
                    End If
                Loop

                'FIND LEFT
                inc = 0
                Dim intLeftPixel As Integer
                Do
                    inc += (PixWidth - 1)
                    intLeftPixel = intN + inc
                    If intLeftPixel > MaxPix Then 'intLeftPixel Mod PixWidth = 0 Then
                        intLeftPixel -= (PixWidth - 1)
                        Exit Do
                    End If

                    'WHITE
                    If JustArr(intLeftPixel) = 0 Then
                        If Not WasWhiteOrUniqDirection Then
                            WasWhiteOrUniqDirection = True
                        Else
                            Exit Do
                        End If

                        'ALTERNATE COLOR OR GRAY
                    ElseIf JustArr(intLeftPixel) = AlternateColor OrElse JustArr(intLeftPixel) = 3 Then
                        If RightVelocityArr(intLeftPixel).Direction <> LeftVelocityArr(intLeftPixel).Direction Then
                            If Not WasWhiteOrUniqDirection Then
                                WasWhiteOrUniqDirection = True
                            Else
                                Exit Do
                            End If
                        End If
                        intLeftPixel -= (PixWidth - 1)
                        ReDrawGray = True
                        Exit Do

                        'SAME COLOR
                    Else
                        If WasWhiteOrUniqDirection Then Exit Do
                    End If
                Loop
                If ReDrawGray Then
                    For NewGrayPixel As Integer = intLeftPixel To intRightPixel Step -1 * (PixWidth - 1)
                        If NewGrayPixel < intRightPixel Then Exit For
                        FinArr(NewGrayPixel) = 3
                        apptools.GrayRemoved += 1
                    Next
                End If
            End If
            If Not ReDrawGray Then
                FinArr(intN) = JustArr(intN)
            End If
        Next

        Return FinArr

    End Function
    Public Function RemoveSpecifiedAdjacents(ByVal JustArr() As Integer, ByVal LeftVelocityArr() As EdgeVector, ByVal RightVelocityArr() As EdgeVector, ByVal SpecifiedDirection As Integer) As Integer()
        Dim FinArr(UBound(JustArr)) As Integer
        Dim PixHeight As Integer = 192
        Dim PixWidth As Integer = 282


        For intN As Integer = 0 To UBound(FinArr) - 1
            'IF COLOR IS GREEN OR RED, THEN GET DIRECTION FROM THE APPROPRIATE VECTOR ARRAY
            Dim ReDrawGray As Boolean = False
            Dim ThisDirection As Integer
            Dim AlternateColor As Integer
            If JustArr(intN) = 2 Or JustArr(intN) = 1 Then
                If JustArr(intN) = 1 Then
                    ThisDirection = LeftVelocityArr(intN).Direction
                    AlternateColor = 2
                ElseIf JustArr(intN) = 2 Then
                    ThisDirection = RightVelocityArr(intN).Direction
                    AlternateColor = 1
                End If

                'NOT VERTICAL DIAGONAL - IGNORE IT AND MOVE ON
                If ThisDirection <> SpecifiedDirection Then
                    FinArr(intN) = JustArr(intN)
                    Continue For
                End If

                Dim PixelDelta As Integer
                Select Case SpecifiedDirection
                    Case 0
                        PixelDelta = 1
                    Case 45
                        PixelDelta = 0 - (PixWidth - 1)
                    Case 90
                        PixelDelta = PixWidth
                    Case 135
                        PixelDelta = (PixWidth + 1)
                End Select

                'OK - HERE I'VE LOCATED A RED OR GREEN PIXEL WHOSE VELOCITY IS NOT VERTICAL
                'FIND RIGHT
                Dim inc As Integer = 0
                Dim intStartPixel As Integer
                Dim WasWhiteOrUniqDirection As Boolean = False
                Do
                    'NEXT PIXEL RIGHT
                    inc += PixelDelta
                    intStartPixel = intN + inc
                    If (intStartPixel + 1) Mod PixWidth = 0 Then
                        intStartPixel -= PixelDelta
                        Exit Do
                    End If

                    'PIXEL IS WHITE
                    If JustArr(intStartPixel) = 0 Then
                        'ALLOW ONE WHITE LINE BETWEEN EDGES WITHOUT DECLARING ERROR
                        If Not WasWhiteOrUniqDirection Then
                            WasWhiteOrUniqDirection = True
                        Else
                            Exit Do
                        End If

                        'PIXEL IS ALTERNATE COLOR OR GRAY
                    ElseIf JustArr(intStartPixel) = AlternateColor OrElse JustArr(intStartPixel) = 3 Then
                        If RightVelocityArr(intStartPixel).Direction <> LeftVelocityArr(intStartPixel).Direction Then
                            If Not WasWhiteOrUniqDirection Then
                                WasWhiteOrUniqDirection = True
                            Else
                                Exit Do
                            End If
                        End If
                        intStartPixel -= PixelDelta
                        ReDrawGray = True
                        Exit Do

                        'SAME COLOR
                    Else
                        If WasWhiteOrUniqDirection Then Exit Do
                    End If
                Loop

                'FIND LEFT
                inc = 0
                Dim intStopPixel As Integer
                Do
                    inc -= PixelDelta
                    intStopPixel = intN + inc
                    If intStopPixel Mod PixWidth = 0 Then
                        intStopPixel += PixelDelta
                        Exit Do
                    End If

                    'WHITE
                    If JustArr(intStopPixel) = 0 Then
                        If Not WasWhiteOrUniqDirection Then
                            WasWhiteOrUniqDirection = True
                        Else
                            Exit Do
                        End If

                        'ALTERNATE COLOR OR GRAY
                    ElseIf JustArr(intStopPixel) = AlternateColor OrElse JustArr(intStopPixel) = 3 Then
                        If RightVelocityArr(intStopPixel).Direction <> LeftVelocityArr(intStopPixel).Direction Then
                            If Not WasWhiteOrUniqDirection Then
                                WasWhiteOrUniqDirection = True
                            Else
                                Exit Do
                            End If
                        End If
                        intStopPixel += PixelDelta
                        ReDrawGray = True
                        Exit Do

                        'SAME COLOR
                    Else
                        If WasWhiteOrUniqDirection Then Exit Do
                    End If
                Loop

                Select Case SpecifiedDirection
                    Case 0

                    Case 45

                    Case 90

                    Case 135

                End Select
                'If ReDrawGray Then
                '    For NewGrayPixel As Integer = intStopPixel To intStartPixel Step PixWidth + 1
                '        If NewGrayPixel > intStartPixel Then Exit For
                '        FinArr(NewGrayPixel) = 3
                '    Next
                'End If
            End If
            If Not ReDrawGray Then
                FinArr(intN) = JustArr(intN)
            End If
        Next

        Return FinArr

    End Function
    Public Function Remove135DegreeAdjacents(ByVal JustArr() As Integer, LeftVelocityArr() As EdgeVector, RightVelocityArr() As EdgeVector) As Integer()

        Dim FinArr(UBound(JustArr)) As Integer
        Dim PixHeight As Integer = 192
        Dim PixWidth As Integer = 282
        Dim PixMax As Integer = PixWidth * PixHeight - 1


        For intN As Integer = 0 To UBound(FinArr) - 1
            'IF COLOR IS GREEN OR RED, THEN GET DIRECTION FROM THE APPROPRIATE VECTOR ARRAY
            Dim ReDrawGray As Boolean = False
            Dim ThisDirection As Integer
            Dim AlternateColor As Integer
            If JustArr(intN) = 2 Or JustArr(intN) = 1 Then
                If JustArr(intN) = 1 Then
                    ThisDirection = LeftVelocityArr(intN).Direction
                    AlternateColor = 2
                ElseIf JustArr(intN) = 2 Then
                    ThisDirection = RightVelocityArr(intN).Direction
                    AlternateColor = 1
                End If

                'NOT VERTICAL DIAGONAL - IGNORE IT AND MOVE ON
                If ThisDirection <> 135 Then
                    FinArr(intN) = JustArr(intN)
                    Continue For
                End If

                'OK - HERE I'VE LOCATED A RED OR GREEN PIXEL WHOSE VELOCITY IS NOT VERTICAL
                'FIND RIGHT
                Dim inc As Integer = 0
                Dim intRightPixel As Integer
                Dim WasWhiteOrUniqDirection As Boolean = False
                Do
                    'NEXT PIXEL RIGHT
                    inc += (PixWidth + 1)
                    intRightPixel = intN + inc
                    If intRightPixel > PixMax Then '(intRightPixel + 1) Mod PixWidth = 0 Then
                        intRightPixel -= (PixWidth + 1)
                        Exit Do
                    End If

                    'PIXEL IS WHITE
                    If JustArr(intRightPixel) = 0 Then
                        'ALLOW ONE WHITE LINE BETWEEN EDGES WITHOUT DECLARING ERROR
                        If Not WasWhiteOrUniqDirection Then
                            WasWhiteOrUniqDirection = True
                        Else
                            Exit Do
                        End If

                        'PIXEL IS ALTERNATE COLOR OR GRAY
                    ElseIf JustArr(intRightPixel) = AlternateColor OrElse JustArr(intRightPixel) = 3 Then
                        If RightVelocityArr(intRightPixel).Direction <> LeftVelocityArr(intRightPixel).Direction Then
                            If Not WasWhiteOrUniqDirection Then
                                WasWhiteOrUniqDirection = True
                            Else
                                Exit Do
                            End If
                        End If
                        intRightPixel -= (PixWidth + 1)
                        ReDrawGray = True
                        Exit Do

                        'SAME COLOR
                    Else
                        If WasWhiteOrUniqDirection Then Exit Do
                    End If
                Loop

                'FIND LEFT
                inc = 0
                Dim intLeftPixel As Integer
                Do
                    inc -= (PixWidth + 1)
                    intLeftPixel = intN + inc
                    If intLeftPixel < 0 Then 'intLeftPixel Mod PixWidth = 0 Then
                        intLeftPixel += (PixWidth + 1)
                        Exit Do
                    End If

                    'WHITE
                    If JustArr(intLeftPixel) = 0 Then
                        If Not WasWhiteOrUniqDirection Then
                            WasWhiteOrUniqDirection = True
                        Else
                            Exit Do
                        End If

                        'ALTERNATE COLOR OR GRAY
                    ElseIf JustArr(intLeftPixel) = AlternateColor OrElse JustArr(intLeftPixel) = 3 Then
                        If RightVelocityArr(intLeftPixel).Direction <> LeftVelocityArr(intLeftPixel).Direction Then
                            If Not WasWhiteOrUniqDirection Then
                                WasWhiteOrUniqDirection = True
                            Else
                                Exit Do
                            End If
                        End If
                        intLeftPixel += (PixWidth + 1)
                        ReDrawGray = True
                        Exit Do

                        'SAME COLOR
                    Else
                        If WasWhiteOrUniqDirection Then Exit Do
                    End If
                Loop
                If ReDrawGray Then
                    For NewGrayPixel As Integer = intLeftPixel To intRightPixel Step PixWidth + 1
                        If NewGrayPixel > intRightPixel Then Exit For
                        FinArr(NewGrayPixel) = 3
                        apptools.GrayRemoved += 1
                    Next
                End If
            End If
            If Not ReDrawGray Then
                FinArr(intN) = JustArr(intN)
            End If
        Next

        Return FinArr

    End Function
    Public Function RemoveVerticalAdjacents(ByVal JustArr() As Integer, LeftVelocityArr() As EdgeVector, RightVelocityArr() As EdgeVector) As Integer()
        Dim FinArr(UBound(JustArr)) As Integer
        Dim PixHeight As Integer = 192
        Dim PixWidth As Integer = 282


        For intN As Integer = 0 To UBound(FinArr) - 1
            'IF COLOR IS GREEN OR RED, THEN GET DIRECTION FROM THE APPROPRIATE VECTOR ARRAY
            Dim ReDrawGray As Boolean = False
            Dim ThisDirection As Integer
            Dim AlternateColor As Integer
            If JustArr(intN) = 2 Or JustArr(intN) = 1 Then
                If JustArr(intN) = 1 Then
                    ThisDirection = LeftVelocityArr(intN).Direction
                    AlternateColor = 2
                ElseIf JustArr(intN) = 2 Then
                    ThisDirection = RightVelocityArr(intN).Direction
                    AlternateColor = 1
                End If

                'NOT VERTICAL DIAGONAL - IGNORE IT AND MOVE ON
                If ThisDirection <> 0 Then
                    FinArr(intN) = JustArr(intN)
                    Continue For
                End If

                'OK - HERE I'VE LOCATED A RED OR GREEN PIXEL WHOSE VELOCITY IS NOT VERTICAL
                'FIND RIGHT
                Dim inc As Integer = 0
                Dim intRightPixel As Integer
                Dim WasWhiteOrUniqDirection As Boolean = False
                Do
                    'NEXT PIXEL RIGHT
                    inc += 1
                    intRightPixel = intN + inc
                    If (intRightPixel + 1) Mod PixWidth = 0 Then
                        intRightPixel -= 1
                        Exit Do
                    End If

                    'PIXEL IS WHITE
                    If JustArr(intRightPixel) = 0 Then
                        'ALLOW ONE WHITE LINE BETWEEN EDGES WITHOUT DECLARING ERROR
                        If Not WasWhiteOrUniqDirection Then
                            WasWhiteOrUniqDirection = True
                        Else
                            Exit Do
                        End If

                        'PIXEL IS ALTERNATE COLOR OR GRAY
                    ElseIf JustArr(intRightPixel) = AlternateColor OrElse JustArr(intRightPixel) = 3 Then
                        If RightVelocityArr(intRightPixel).Direction <> LeftVelocityArr(intRightPixel).Direction Then
                            If Not WasWhiteOrUniqDirection Then
                                WasWhiteOrUniqDirection = True
                            Else
                                Exit Do
                            End If
                        End If
                        intRightPixel -= 1
                        ReDrawGray = True
                        Exit Do

                        'SAME COLOR
                    Else
                        If WasWhiteOrUniqDirection Then Exit Do
                    End If
                Loop

                'FIND LEFT
                inc = 0
                Dim intLeftPixel As Integer
                Do
                    inc -= 1
                    intLeftPixel = intN + inc
                    If intLeftPixel Mod PixWidth = 0 Then
                        intLeftPixel += 1
                        Exit Do
                    End If

                    If JustArr(intLeftPixel) = 0 Then 'white
                        If Not WasWhiteOrUniqDirection Then
                            WasWhiteOrUniqDirection = True
                        Else
                            Exit Do
                        End If

                        'ALTERNATE COLOR OR GRAY
                    ElseIf JustArr(intLeftPixel) = AlternateColor OrElse JustArr(intLeftPixel) = 3 Then
                        If RightVelocityArr(intLeftPixel).Direction <> LeftVelocityArr(intLeftPixel).Direction Then
                            If Not WasWhiteOrUniqDirection Then
                                WasWhiteOrUniqDirection = True
                            Else
                                Exit Do
                            End If
                        End If
                        intLeftPixel += 1
                        ReDrawGray = True
                        Exit Do

                    Else
                        If WasWhiteOrUniqDirection Then Exit Do
                    End If
                Loop
                If ReDrawGray Then
                    For NewGrayPixel As Integer = intLeftPixel To intRightPixel
                        FinArr(NewGrayPixel) = 3
                        AppTools.GrayRemoved += 1
                    Next
                End If
            End If
            If Not ReDrawGray Then
                FinArr(intN) = JustArr(intN)
            End If
        Next

        Return FinArr

    End Function
    Public Function WAS_ErrorWeighting(ByVal PixArr() As Integer, ByRef LargestOutline As Integer) As Integer
        'hELLO
        Dim PixWidth As Integer = 282
        Dim PixHeight As Integer = 192
        Dim FinalAnswer As Integer

        Dim ErrNo As Integer
        Dim ExaminedPixels(UBound(PixArr)) As Integer
        'CLEAR THE ERROR GRID
        Try
            For intN As Integer = 0 To UBound(PixArr)
                'LOOK AT ALL UNEXAMINED HORIZONTAL PIXELS
                If ExaminedPixels(intN) = 0 Then
                    Dim intColor As Integer = PixArr(intN)
                    '0 = white, 1 = green, 2 = red, 3 = gray
                    If intColor = 0 OrElse intColor = 3 Then
                        ExaminedPixels(intN) = 10000
                        Continue For
                    Else 'first red or green found
                        Dim LastHorzBox As Integer = 0
                        Dim FirstHorzBox As Integer = 0
                        Dim TotalBoxes As Integer = 0
                        Dim incX As Integer = 0
                        Do
                            Dim NextIntN As Integer = intN + incX
                            If (NextIntN + 1) Mod PixWidth = 0 Then Exit Do
                            If BoxVerified(PixArr, ExaminedPixels, intColor, NextIntN, ErrNo, False) Then
                                If FirstHorzBox = 0 Then FirstHorzBox = intN
                                LastHorzBox = NextIntN
                                TotalBoxes += 1
                            Else : Exit For
                            End If
                            incX += 1
                        Loop

                        If LastHorzBox = 0 Then Continue For 'exit without weighting if no box found
                        Dim EarliestBoxOnAnyRow As Integer = FirstHorzBox
                        Dim LatestBoxOnAnyRow As Integer = LastHorzBox
                        Dim FirstRow As Integer = intN
                        Dim LastRow As Integer = intN
                        Dim incY As Integer = PixWidth
                        Do
                            Dim NextIntY As Integer = intN + incY
                            If NextIntY > UBound(PixArr) - 1 OrElse UBound(PixArr) - 1 - NextIntY < PixWidth Then Continue For
                            Dim NewBoxesInRow As Integer = 0
                            'EXAMINE THE AREA UNDER EACH FOUND BOX FOR THE EXISTENCE OF ANOTHER BOX
                            Dim NextFirstBox As Integer = 0
                            Dim NextLastBox As Integer = 0
                            For NextRowIntX As Integer = NextIntY To NextIntY + (LastHorzBox - FirstHorzBox)
                                If ExaminedPixels(NextRowIntX) <> ErrNo Then Continue For
                                If BoxVerified(PixArr, ExaminedPixels, intColor, NextRowIntX, ErrNo, False) Then
                                    If NextFirstBox = 0 Then NextFirstBox = NextRowIntX
                                    NextLastBox = NextRowIntX
                                    NewBoxesInRow += 1
                                End If
                            Next

                            'CHECK AREA TO THE LEFT OF THIS ROW'S EARLIEST BOX - RESET EARLIEST BOX IF NECESSARY
                            If NextFirstBox = FirstHorzBox Then
                                For NextRowIntX As Integer = NextFirstBox To 1 Step -1
                                    If BoxVerified(PixArr, ExaminedPixels, intColor, NextRowIntX, ErrNo, False) Then
                                        If NextRowIntX < EarliestBoxOnAnyRow Then EarliestBoxOnAnyRow = NextRowIntX
                                        NextFirstBox = NextRowIntX
                                        NewBoxesInRow += 1
                                    Else : Exit For
                                    End If
                                Next
                            End If
                            'CHECK THE AREA TO THE RIGHT OF THIS ROW'S FINAL BOX - RESET FINAL BOX IF NECESSARY
                            If NextLastBox = LastHorzBox Then
                                'For NextRowIntX As Integer = LastHorzBox To TargetWidth - 2
                                '    If BoxVerified(PixArr, ExaminedPixels, intColor, NextRowIntX, ErrNo, False) Then
                                '        If NextRowIntX > LatestBoxOnAnyRow Then LatestBoxOnAnyRow = NextRowIntX
                                '        NextLastBox = NextRowIntX
                                '        NewBoxesInRow += 1
                                '    Else : Exit For
                                '    End If
                                'Next
                            End If

                            If NewBoxesInRow = 0 Then Exit For
                            TotalBoxes += NewBoxesInRow
                            LastRow = NextIntY
                            FirstHorzBox = NextFirstBox
                            LastHorzBox = NextLastBox

                            incY += PixWidth
                        Loop

                        LargestOutline = Math.Max(LargestOutline, TotalBoxes)

                        'GET THE AVERAGE HEIGHT AND WIDTH THEN WEIGHT THE ERROR
                        'Dim AbsoluteWidth As Integer = LatestBoxOnAnyRow - EarliestBoxOnAnyRow
                        'Dim AbsoluteHeight As Integer = LastRow - FirstRow
                        'If AbsoluteHeight > 0 And AbsoluteWidth > 0 Then
                        '    Dim AvgWidth As Decimal = TotalBoxes / AbsoluteWidth
                        '    Dim AvgHeight As Decimal = TotalBoxes / AbsoluteHeight
                        '    Dim WidthWeight As Decimal = Math.Min((AvgWidth * AvgWidth), 100)
                        '    Dim heightweight As Decimal = Math.Min((AvgHeight * AvgHeight), 100)

                        '    Dim MinimumDimension As Decimal = Math.Min(WidthWeight, heightweight)
                        '    FinalAnswer += (TotalBoxes * MinimumDimension)
                        'End If
                    End If
                End If
            Next

            Return FinalAnswer

        Catch ex As Exception
            Trace.WriteLine("sorbeledgedetect.OvertFramingErrors -> " & ex.Message)
            Return Nothing
        Finally

        End Try
    End Function
    Public Function BoxVerified(ByVal inpArr() As Integer, ByRef ExaminedPixels() As Integer, ByVal ColorInteger As Integer, ByVal Coord As Integer, ByRef ErrNo As Integer, ByVal KeepErr As Boolean) As Boolean
        'VERIFIES THE PIXEL BOX IS HOMOGENOUS AND OF THE CORRECT ERROR COLOR
        'IF TRUE IT ALSO SETS ALL PIXEL VALUES TO EXAMINED
        Dim PixWidth As Integer = 282
        Dim PixHeight As Integer = 192
        Dim RightPixel As Integer = inpArr(Coord + 1)
        Dim DownPixel As Integer = inpArr(Coord + PixWidth)
        Dim Right_DownPixel As Integer = inpArr(Coord + PixWidth + 1)


        Dim BoxIsTrue As Boolean
        If RightPixel = ColorInteger AndAlso DownPixel = ColorInteger AndAlso Right_DownPixel = ColorInteger Then
            Dim FoundCorrelation As Boolean
            Dim CompareUpperUpperLeft As Integer = inpArr(Coord - PixWidth - 1)
            If CompareUpperUpperLeft <> ColorInteger AndAlso CompareUpperUpperLeft <> 0 Then
                FoundCorrelation = True
            End If

            If Not FoundCorrelation Then
                Dim CompareUpperUpperRight As Integer = inpArr(Coord - PixWidth + 1)
                If CompareUpperUpperRight <> ColorInteger AndAlso CompareUpperUpperRight <> 0 Then
                    FoundCorrelation = True
                End If
            End If

            If Not FoundCorrelation Then
                Dim CompareLowerLowerLeft As Integer = inpArr(Coord + (2 * PixWidth) - 1)
                If CompareLowerLowerLeft <> ColorInteger AndAlso CompareLowerLowerLeft <> 0 Then
                    FoundCorrelation = True
                End If
            End If
            If Not FoundCorrelation Then
                Dim CompareLowerLowerRight As Integer = inpArr(Coord + (2 * PixWidth) + 1)
                If CompareLowerLowerRight <> ColorInteger AndAlso CompareLowerLowerRight <> 0 Then
                    FoundCorrelation = True
                End If
            End If

            If Not FoundCorrelation Then BoxIsTrue = True
        End If

        If BoxIsTrue Then
            If Not KeepErr Then ErrNo += 1
            ExaminedPixels(Coord) = ErrNo
            ExaminedPixels(Coord + PixWidth) = ErrNo
            ExaminedPixels(Coord + 1) = ErrNo
            ExaminedPixels(Coord + PixWidth + 1) = ErrNo
            Return True
        End If

        Return False
    End Function

    Public Function RemoveOutliers(InArr() As Integer) As Integer()
        Dim NewArr(UBound(InArr)) As Integer
        Dim PixWidth As Integer = 282
        Dim PixHeight As Integer = 192

        Try
            For intN As Integer = 0 To UBound(InArr) - 1
                Dim RedrawGray As Boolean = False
                If InArr(intN) = 1 Then 'green
                    For intNNext As Integer = intN - PixWidth To intN + PixWidth Step PixWidth
                        If intNNext = intN Then Continue For
                        If intNNext > UBound(InArr) OrElse intNNext < 0 Then Continue For
                        Dim AbuttingColor As Integer = InArr(intNNext)
                        If AbuttingColor = 3 OrElse AbuttingColor = 2 Then 'gray or red
                            RedrawGray = True
                            Exit For
                        End If
                    Next
                    If Not RedrawGray Then
                        For intnNext As Integer = intN - 1 To intN + 1
                            If intnNext = intN Then Continue For
                            If intnNext > UBound(InArr) OrElse intnNext < 0 Then Continue For
                            Dim AbuttingColor As Integer = InArr(intnNext)
                            If AbuttingColor = 3 OrElse AbuttingColor = 2 Then 'gray or red
                                RedrawGray = True
                                Exit For
                            End If
                        Next
                    End If
                ElseIf InArr(intN) = 2 Then 'red
                    For intnNext As Integer = intN - PixWidth To intN + PixWidth Step PixWidth
                        If intnNext = intN Then Continue For
                        If intnNext > UBound(InArr) OrElse intnNext < 0 Then Continue For
                        Dim AbuttingColor As Integer = InArr(intnNext)
                        If AbuttingColor = 3 OrElse AbuttingColor = 1 Then 'gray or green
                            RedrawGray = True
                            Exit For
                        End If
                    Next
                    If Not RedrawGray Then
                        For intnNext As Integer = intN - 1 To intN + 1
                            If intnNext = intN Then Continue For
                            If intnNext > UBound(InArr) OrElse intnNext < 0 Then Continue For
                            Dim AbuttingColor As Integer = InArr(intnNext)
                            If AbuttingColor = 3 OrElse AbuttingColor = 1 Then 'gray or green
                                RedrawGray = True
                                Exit For
                            End If
                        Next
                    End If
                End If

                If RedrawGray Then
                    NewArr(intN) = 3
                Else
                    NewArr(intN) = InArr(intN)
                End If
            Next

            Return NewArr
        Catch ex As Exception
            Trace.WriteLine("SorbelEdgeDetect:RemoveOutliers -> " & ex.Message)
            Return Nothing
        Finally

        End Try

    End Function

    Public Function ArrayIntoBitmap(ByVal PixArray() As Integer, Optional ByVal Wdth As Integer = 0, Optional ByVal HGT As Integer = 0, Optional ByVal XZones As Boolean = False) As Bitmap
        Dim PixWidth As Integer = 282
        If Wdth > 0 Then PixWidth = Wdth
        Dim PixHeight As Integer = 192
        If HGT > 0 Then PixHeight = HGT
        Dim ReturnBmp As New Bitmap(PixWidth, PixHeight)

        Dim rectangleBounds As New Rectangle(0, 0, PixWidth, PixHeight)

        Dim dataReturn As BitmapData
        dataReturn = ReturnBmp.LockBits(rectangleBounds, ImageLockMode.WriteOnly, PixelFormat.Format24bppRgb)
        Dim PointerReturn As IntPtr = dataReturn.Scan0

        For intN As Integer = 0 To UBound(PixArray) - 1
            Dim intx As Integer = Math.Truncate(intN / PixHeight)
            Dim inty As Integer = intN - (intx * PixHeight)
            Dim intOffset As Integer = dataReturn.Stride * inty + (3 * intx)
            If XZones Then
                With AppTools.ExcludeZone
                    If .FirstZone = True Then
                        If intx > .FirstLeft AndAlso intx < .FirstRight AndAlso inty > .FirstTop AndAlso inty < .FirstBottom Then
                            Marshal.WriteByte(PointerReturn, intOffset, 230)
                            Marshal.WriteByte(PointerReturn, intOffset + 1, 215)
                            Marshal.WriteByte(PointerReturn, intOffset + 2, 175)
                            Continue For
                        End If
                    End If
                    If .SecondZone = True Then
                        If intx > .SecondLeft AndAlso intx < .SecondRight AndAlso inty > .SecondTop AndAlso inty < .SecondBottom Then
                            Marshal.WriteByte(PointerReturn, intOffset, 230)
                            Marshal.WriteByte(PointerReturn, intOffset + 1, 215)
                            Marshal.WriteByte(PointerReturn, intOffset + 2, 175)
                            Continue For
                        End If
                    End If
                End With
            End If
            Select Case PixArray(intN)
                Case 1 'green
                    Marshal.WriteByte(PointerReturn, intOffset, 0)
                    Marshal.WriteByte(PointerReturn, intOffset + 1, 128)
                    Marshal.WriteByte(PointerReturn, intOffset + 2, 0)
                Case 2 ' red
                    Marshal.WriteByte(PointerReturn, intOffset, 0)
                    Marshal.WriteByte(PointerReturn, intOffset + 1, 0)
                    Marshal.WriteByte(PointerReturn, intOffset + 2, 128)
                Case 3 ' gray
                    Marshal.WriteByte(PointerReturn, intOffset, 200)
                    Marshal.WriteByte(PointerReturn, intOffset + 1, 200)
                    Marshal.WriteByte(PointerReturn, intOffset + 2, 200)
                Case 0 'white
                    Marshal.WriteByte(PointerReturn, intOffset, 255)
                    Marshal.WriteByte(PointerReturn, intOffset + 1, 255)
                    Marshal.WriteByte(PointerReturn, intOffset + 2, 255)
            End Select
        Next

        ReturnBmp.UnlockBits(dataReturn)

        Return ReturnBmp
    End Function

    Public Sub FastFramingDelta(ByVal intBmpWidth As Integer, ByVal intBmpHeight As Integer)
        Dim TotDelta As Integer
        Dim GrayShade As Integer
        Dim intTenPercentWidth As Integer = Convert.ToInt32(intBmpWidth / 10)
        Dim intTenPercentHeight As Integer = Convert.ToInt32(intBmpHeight / 10)
        ReDim CompareVectorArray(intBmpWidth, intBmpHeight)
        For intX As Integer = 0 To intBmpWidth - 1
            For intY As Integer = 0 To intBmpHeight - 1
                If intX < intTenPercentWidth OrElse intX > intBmpWidth - intTenPercentWidth OrElse intY < intTenPercentHeight OrElse intY > intBmpHeight - intTenPercentHeight Then
                    CompareVectorArray(intX, intY) = 0 'white
                    Continue For
                End If

                Dim Left As Boolean = False
                Dim EdgePoint As EdgeVector = LeftVectorArray(intX, intY)
                If EdgePoint.Magnitude > 0 Then Left = True

                Dim Right As Boolean = False
                EdgePoint = RightVectorArray(intX, intY)
                If EdgePoint.Magnitude > 0 Then Right = True

                If Left AndAlso Not Right Then
                    CompareVectorArray(intX, intY) = 1 'green
                ElseIf Right AndAlso Not Left Then
                    CompareVectorArray(intX, intY) = 2 'red
                ElseIf Right AndAlso Left Then
                    CompareVectorArray(intX, intY) = 3 'gray
                    GrayShade += 1
                Else
                    CompareVectorArray(intX, intY) = 0 'white
                End If

                Dim ThisDelta As Integer = Math.Abs(CInt(Left) - CInt(Right))
                TotDelta += ThisDelta
            Next
        Next

    End Sub

    ''' <summary>
    ''' In the ideal case, the result of applying an edge detector to an image may lead to a set of connected curves that indicate the boundaries of objects, 
    ''' the boundaries of surface markings as well as curves that correspond to discontinuities in surface orientation. Thus, applying an edge detection algorithm 
    ''' to an image may significantly reduce the amount of data to be processed and may therefore filter out information that may be regarded as less relevant, 
    ''' while preserving the important structural properties of an image. If the edge detection step is successful, the subsequent task of interpreting the 
    ''' information contents in the original image may therefore be substantially simplified. However, it is not always possible to obtain such ideal edges from 
    ''' real life images of moderate complexity.
    ''' </summary>
    ''' <param name="bmp"></param>
    ''' <param name="Threshold"></param>
    ''' <param name="AllBlack"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Public Function FastEdgeDetectNew(ByVal bmp As Bitmap, ByVal Threshold As Integer, Optional ByVal AllBlack As Integer = 0) As EdgeVector()
        Try
            Dim LuminRow(0 To 2) As Integer
            Dim Lumin2Row(0 To 2) As Integer
            Dim Lumin3Row(0 To 2) As Integer
            Dim intGradientX As Integer = 0
            Dim intGradientY As Integer = 0

            Dim SorbelVelocityArr(0 To bmp.Width * bmp.Height) As EdgeVector

            Dim rectangleBounds As New Rectangle(0, 0, bmp.Width, bmp.Height)

            Dim workingBitmap As Bitmap = bmp
            Dim dataBitmap As BitmapData
            dataBitmap = workingBitmap.LockBits(rectangleBounds, ImageLockMode.ReadOnly, PixelFormat.Format24bppRgb)
            Dim Pointer As IntPtr = dataBitmap.Scan0

            Dim SorbelBmp As New Bitmap(bmp.Width, bmp.Height)
            Dim dataReturn As BitmapData
            dataReturn = SorbelBmp.LockBits(rectangleBounds, ImageLockMode.WriteOnly, PixelFormat.Format24bppRgb)
            Dim PointerReturn As IntPtr = dataReturn.Scan0
            Dim intN As Integer = 0
            For intX As Integer = 0 To workingBitmap.Width - 1
                For intY As Integer = 0 To workingBitmap.Height - 1
                    Dim intOffset As Integer = dataBitmap.Stride * intY + (3 * intX)
                    Dim VectorItem As EdgeVector
                    If (intX < 1 OrElse intX > bmp.Width - 2 OrElse intY < 1 OrElse intY > bmp.Height - 2) Then
                        VectorItem.Direction = 180
                        VectorItem.Magnitude = 0
                        SorbelVelocityArr(intN) = VectorItem 'Theta
                    Else
                        intGradientX = 0
                        intGradientY = 0

                        'first horizontal row
                        Dim intx2 As Integer = intX - 1
                        Dim inty2 As Integer = intY - 1
                        intOffset = dataBitmap.Stride * inty2 + (3 * intx2)
                        LuminRow(0) = Marshal.ReadByte(Pointer, intOffset)
                        intx2 = intX
                        intOffset = dataBitmap.Stride * inty2 + (3 * intx2)
                        LuminRow(1) = Marshal.ReadByte(Pointer, intOffset)
                        intx2 = intX + 1
                        intOffset = dataBitmap.Stride * inty2 + (3 * intx2)
                        LuminRow(2) = Marshal.ReadByte(Pointer, intOffset)

                        'second horizontal row
                        intx2 = intX - 1
                        inty2 = intY
                        intOffset = dataBitmap.Stride * inty2 + (3 * intx2)
                        Lumin2Row(0) = Marshal.ReadByte(Pointer, intOffset)
                        intx2 = intX + 1
                        intOffset = dataBitmap.Stride * inty2 + (3 * intx2)
                        Lumin2Row(2) = Marshal.ReadByte(Pointer, intOffset)

                        'third horizontal row
                        intx2 = intX - 1
                        inty2 = intY + 1
                        intOffset = dataBitmap.Stride * inty2 + (3 * intx2)
                        Lumin3Row(0) = Marshal.ReadByte(Pointer, intOffset)
                        intx2 = intX
                        intOffset = dataBitmap.Stride * inty2 + (3 * intx2)
                        Lumin3Row(1) = Marshal.ReadByte(Pointer, intOffset)
                        intx2 = intX + 1
                        intOffset = dataBitmap.Stride * inty2 + (3 * intx2)
                        Lumin3Row(2) = Marshal.ReadByte(Pointer, intOffset)

                        'calculate X, Y and final gradients
                        intGradientX = LuminRow(2) - LuminRow(0)
                        intGradientX += 2 * (Lumin2Row(2) - Lumin2Row(0))
                        intGradientX += Lumin3Row(2) - Lumin3Row(0)
                        intGradientY = Lumin3Row(0) - LuminRow(0)
                        intGradientY += 2 * (Lumin3Row(1) - LuminRow(1))
                        intGradientY += Lumin3Row(2) - LuminRow(2)

                        Dim GradientMagnitude As Integer = Math.Abs(intGradientY) + Math.Abs(intGradientX)

                        Dim Theta As Integer = 180
                        If intGradientX <> 0 Then
                            Dim Division As Decimal = intGradientY / intGradientX
                            Theta = (Math.Atan2(intGradientY, intGradientX) / Math.PI) * 180
                            If Theta < 0 Then Theta = 180 + Theta
                        Else
                            If intGradientY = 0 Then
                                Theta = 0
                            Else : Theta = 90
                            End If
                        End If

                        If (Theta >= 0 And Theta <= 22.5) Or Theta > (157.5 And Theta <= 180) Then
                            Theta = 0
                        ElseIf Theta > 22.5 And Theta <= 67.5 Then
                            Theta = 45
                        ElseIf (Theta > 67.5 And Theta <= 112.5) Then
                            Theta = 90
                        ElseIf Theta > 112.5 And Theta < 157.5 Then
                            Theta = 135
                        End If

                        If GradientMagnitude <= Threshold Then
                            VectorItem.Magnitude = 0
                            VectorItem.Direction = 180
                            SorbelVelocityArr(intN) = VectorItem 'Theta
                        Else
                            VectorItem.Direction = Theta
                            VectorItem.Magnitude = GradientMagnitude
                            SorbelVelocityArr(intN) = VectorItem 'Theta

                            Dim Grayshade As Byte = 255 - CInt(GradientMagnitude * 255 / 2040)
                        End If
                    End If
                    intN += 1
                Next
            Next

            SorbelBmp.UnlockBits(dataReturn)
            workingBitmap.UnlockBits(dataBitmap)
            Return SorbelVelocityArr

        Catch ex As Exception
            Throw
        End Try
    End Function
End Module


