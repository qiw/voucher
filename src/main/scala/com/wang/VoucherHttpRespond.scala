package com.wang


import scala.collection.JavaConversions._
import scala.collection.mutable.Map
import org.slf4j.LoggerFactory

import org.jboss.netty.handler.codec.http.{QueryStringDecoder, HttpRequest, HttpResponse, DefaultHttpResponse, HttpHeaders, HttpResponseStatus, HttpMethod}
import org.jboss.netty.handler.codec.http.HttpVersion.HTTP_1_1
import org.jboss.netty.buffer.ChannelBuffers.copiedBuffer
import org.jboss.netty.util.CharsetUtil.UTF_8

import com.codahale.jerkson.Json._
import com.twitter.finagle.{Service, SimpleFilter}
import com.twitter.util.Future



class InvalidHttpParameter(msg:String) extends Exception(msg)

class UnknowWebServiceRequest extends Exception

class HandleExceptions extends SimpleFilter[HttpRequest, HttpResponse] {
  val log = LoggerFactory.getLogger(getClass.getName)

  def apply(request: HttpRequest, service: Service[HttpRequest, HttpResponse]) = {
    service(request) handle { case error =>
      val statusCode = error match {
        case _: InvalidHttpParameter => HttpResponseStatus.BAD_REQUEST
        case e =>
          log.error("darrrrh", e)
          HttpResponseStatus.INTERNAL_SERVER_ERROR
        }

      new DefaultHttpResponse(HTTP_1_1, statusCode)
    }
  }
}


class VoucherHttpRespond(voucherService: VoucherService) extends Service[HttpRequest, HttpResponse] {
  val log = LoggerFactory.getLogger(getClass.getName)

  def apply(request: HttpRequest) = {
    import java.lang.{ String  => JString}
    import java.util.{ List  => JList}
    
    log.debug(request.getUri)

    val httpMethod = request.getMethod

    val queryDecoder = new QueryStringDecoder(request.getUri)
    val postDecoder = new QueryStringDecoder("?" + request.getContent.toString(org.jboss.netty.util.CharsetUtil.UTF_8))

    val params:Map[JString, JList[JString]] = queryDecoder.getParameters
    val postParams:Map[JString, JList[JString]]  = postDecoder.getParameters

    ((httpMethod, queryDecoder.getPath) match {
      case (HttpMethod.GET, "/voucher") =>
        params.get("code") map { c =>
          voucherService redeemVoucher c.get(0)
        } getOrElse(Future.value(""))

      case (HttpMethod.POST, "/voucher") =>
        val u = postParams.get("user") map(_.get(0)) getOrElse { throw new InvalidHttpParameter("invalid user") }
        val promotionId = postParams.get("pid") map(_.get(0)) getOrElse { throw new InvalidHttpParameter("invalid user") }
        voucherService.createVoucher(u, promotionId.toInt)

      case (HttpMethod.POST, "/promotion") =>
        val u = postParams.get("user") map(_.get(0)) getOrElse { throw new InvalidHttpParameter("invalid user") }
        val game = postParams.get("game") map(_.get(0)) getOrElse { throw new InvalidHttpParameter("invalid game") }
        val vid = postParams.get("vid") map(_.get(0)) getOrElse { throw new InvalidHttpParameter("invalid vid") }
        voucherService.findPromotion(u, game.toInt, vid.toInt)

      case (HttpMethod.GET, "/promotion") =>
        val pid = params.get("pid") map(_.get(0)) getOrElse { throw new InvalidHttpParameter("invalid vid") }
        voucherService.getPromotion(pid.toInt)

      case (HttpMethod.GET, "/inventory") =>
        val u = params.get("user") map(_.get(0)) getOrElse { throw new InvalidHttpParameter("invalid user") }
        val game = params.get("game") map(_.get(0)) getOrElse { throw new InvalidHttpParameter("invalid game") }
        voucherService.getVirtualGoodsInventory(u, game.toInt)

      case (HttpMethod.DELETE, "/inventory") =>
        val u = params.get("user") map(_.get(0)) getOrElse { throw new InvalidHttpParameter("invalid user") }
        val game = params.get("game") map(_.get(0)) getOrElse { throw new InvalidHttpParameter("invalid game") }
        val vid = params.get("vid") map(_.get(0)) getOrElse { throw new InvalidHttpParameter("invalid vid") }
        val amount = params.get("amt") map(_.get(0)) getOrElse { throw new InvalidHttpParameter("invalid amt") }
        
        voucherService.consumeVirtualGoods(u, game.toInt, vid.toInt, amount.toInt)
      case p =>
        log.info("unknow path " + p)
        Future.value("")
        
    }) map { json =>
      val response = new DefaultHttpResponse(HTTP_1_1, HttpResponseStatus.OK)
      response.setHeader(HttpHeaders.Names.CONTENT_TYPE, "application/json")
      response.setContent(copiedBuffer(generate(json), UTF_8))
      response
    }
  }
}

