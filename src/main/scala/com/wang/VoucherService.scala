package com.wang

import javax.xml.bind.DatatypeConverter

import scala.util.Random

import org.slf4j.LoggerFactory

import com.twitter.querulous.evaluator.QueryEvaluator

import com.wang.util.Util

// business logic of voucher service

class VoucherService(queryEvaluator: QueryEvaluator, qrsize:Int) {
  val MAX_QR_SIZE = 480
  val rand = new Random(System.currentTimeMillis)
  val log = LoggerFactory.getLogger(getClass.getName)

  
  /*
   * todo: add security check; only targeted merchant can call this method
   */
  def redeemVoucher(hashCode: String) = {
    queryEvaluator.select("select vouchers.id as vid, user_id, game_id,  virtual_product_id, virtual_product_amount " + 
                          "from vouchers join promotions on vouchers.promotion_id = promotions.id " +
                          "where vouchers.code = ?", hashCode) { rs =>
      (rs.getString("user_id"), rs.getInt("game_id"), rs.getInt("virtual_product_id"), rs.getInt("virtual_product_amount"),
       rs.getInt("vid"))
    }.headOption map { case(user, gameId, pid, amt, vid) =>
      queryEvaluator.transaction { transaction =>
        transaction.execute("insert into inventory (user_id, game_id, product_id, amount) values(?, ?, ?, ?) " +
                            "on duplicate key update amount = amount + values(amount)", user, gameId, pid, amt)
        transaction.execute("delete from vouchers where id = ?", vid)

      }
      Map("description" -> "redeemed")
    } getOrElse(Map("description" -> "invalid voucher"))
  }

  // match a promotion, now hard coded
  def findPromotion(userId: String, gameId:Int, vGoodId:Int) = {

    queryEvaluator.select("select * from promotions limit 1") { rs =>
      (rs.getInt("id"), rs.getString("description"))
    }.headOption map { case(id, desc) =>
      Map("description" -> desc, "promotionId" -> id)
    } getOrElse(Map("description" -> "no promotion found"))
  }


  // used by merchant to display promotion before confirm the voucher
  def getPromotion(promotionId:Int) = {
    queryEvaluator.select("select * from promotions where id = ?", promotionId) { rs =>
      rs.getString("description")
    }.headOption map { d =>
      Map("description" -> d)
    } getOrElse (Map("description" -> "invalid promotion"))
  }
 
  // the code generated should contain the info of merchant, product_id and amout
  // find a better way to do this
  def createVoucher(userId: String, promotionId:Int) = {
    queryEvaluator.select("select * from promotions where id = ?", promotionId) { rs =>
      (rs.getInt("id")) 
    }.headOption map { p_id =>
      // using timestamp and random number to ensure uniqueness of the code
      val code = Util.md5Digest("%s %d %d %d" format(userId, promotionId, System.currentTimeMillis, rand.nextInt))
      queryEvaluator.execute("insert into vouchers (user_id, promotion_id, code) values (?, ?, ?)",
        userId, promotionId, code)
      // make a qr code
      val qr = Util.makeBase64QRCodePNG("%s %d" format(code, p_id), Math.min(MAX_QR_SIZE, qrsize))
      Map("description" -> "voucher created", "code" -> qr)
    } getOrElse {
      Map("description" -> "promotion doesn't exist")
    }
  }


  // return the in game virtual goods list
  def getVirtualGoodsInventory(userId:String, gameId:Int): Map[Int, Int] = {
    queryEvaluator.select("select * from inventory where user_id = ? and game_id = ?", userId, gameId) { rs =>
      (rs.getInt("product_id"), rs.getInt("amount"))
    } toMap
  }

  // user consumes the virtual goods
  // todo: verify input
  def consumeVirtualGoods(userId:String, gameId:Int, vGoodId:Int, amount:Int) {
    // minimum amount is 0, no negative
    queryEvaluator.execute("update inventory set amount = if(amount - ? > 0, amount - ?, 0) " +
                           "where user_id = ? and game_id = ? and product_id = ?",
                           amount, amount, userId, gameId, vGoodId)
  }


}
