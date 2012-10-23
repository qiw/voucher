package com.wang.util

import java.io.ByteArrayOutputStream
import java.security.DigestOutputStream
import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter

import com.google.zxing.BarcodeFormat
import com.google.zxing.MultiFormatWriter
import com.google.zxing.client.j2se.MatrixToImageWriter


object Util {
  def base64Encode(data: Array[Byte]) = {
    DatatypeConverter.printBase64Binary(data)
  }

  def makeBase64QRCodePNG(code:String, size:Int) = {
    val writer = new MultiFormatWriter
    val os = new ByteArrayOutputStream
    MatrixToImageWriter.writeToStream(writer.encode(code, BarcodeFormat.QR_CODE, size, size), "png", os)
    base64Encode(os.toByteArray)
  }

  def md5Digest(data:String) = {
    base64Encode(MessageDigest.getInstance("MD5").digest(data.getBytes))
  }
}
