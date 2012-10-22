package com.wang

import org.slf4j.LoggerFactory


import java.net.InetSocketAddress
import com.twitter.finagle.builder.ServerBuilder

import com.twitter.finagle.http.Http

import com.twitter.querulous.evaluator.QueryEvaluator


object Main {
  def main(args: Array[String]) {

    val respond = new VoucherHttpRespond(new VoucherService(QueryEvaluator(
      "localhost:3306", "voucherdb", "voucher_admin", "123", Map[String, String](), "jdbc:mysql")))
    val handleException = new HandleExceptions

    val server = ServerBuilder()
      .codec(Http())
      .bindTo(new InetSocketAddress(8080))
      .name("httpserver")
      .build(handleException andThen respond)


  }
}
