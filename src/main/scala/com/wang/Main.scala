package com.wang

import org.slf4j.LoggerFactory


import java.net.InetSocketAddress
import com.twitter.finagle.builder.ServerBuilder

import com.twitter.finagle.http.Http

import com.twitter.querulous.evaluator.QueryEvaluator

import scopt.immutable.OptionParser

case class Config (httpport:Int=65000, host:String="localhost", dbport:Int = 3306,
                   db:String="non-exist", dbuser:String="nobody", dbpwd:String="",
                   qrsize:Int=160) 


object Main {
  def main(args: Array[String]) {

    val parser = new OptionParser[Config]("voucher server", "alpha") {
      def options = Seq(
        intOpt("port", "port of http service") { case(p, c) => c.copy(httpport=p) },
        opt("dbhost", "mysql host") { case(h, c) => c.copy(host=h) },
        intOpt("dbport", "mysql port") { case(p, c) => c.copy(dbport=p) },
        opt("db", "db name") { case(d, c) => c.copy(db=d) },
        opt("dbuser", "db user, be sure to have r/w privilege") { case(u, c) => c.copy(dbuser=u) },
        opt("dbpwd", "password") { case(p, c) => c.copy(dbpwd=p) },
        intOpt("qrsize" ,"size of the qr code png") { case (s, c) => c.copy(qrsize=s) }
      )
    }

    parser parse(args, Config()) map { config =>
      val respond = new VoucherHttpRespond(new VoucherService(QueryEvaluator(
        "%s:%d" format(config.host, config.dbport), config.db, config.dbuser, config.dbpwd, Map[String, String](), "jdbc:mysql"),
        config.qrsize))
      val handleException = new HandleExceptions

      val server = ServerBuilder()
        .codec(Http())
        .bindTo(new InetSocketAddress(config.httpport))
        .name("httpserver")
        .build(handleException andThen respond)
    } getOrElse {
      parser.usage
    }

  }
}
