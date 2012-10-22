package com.wang


import scala.util.Random

import com.twitter.querulous.evaluator.QueryEvaluator

// business logic of voucher service

class VoucherService(queryEvaluator: QueryEvaluator) {
  val rand = new Random(System.currentTimeMillis)

  
  /*
   * todo: add security check; only targeted merchant can call this method
   */
  def redeemVoucher(hashCode: String) = {
    queryEvaluator.select("select * from vouchers join promotions " + 
                          "on vouchers.promotion_id = promotions.id " +
                          "where vouchers.code = ?", hashCode) { rs =>
      (rs.getString("user_id"), rs.getInt("game_id"), rs.getInt("virtual_product_id"), rs.getInt("virtual_product_amount"))
    }.headOption map { case(user, gameId, pid, amt) =>
      queryEvaluator.execute("insert into inventory (user_id, game_id, product_id, amount) values(?, ?, ?, ?) " +
                             "on duplicate key update amount = amount + values(amount)", user, gameId, pid, amt)
      Map("description" -> "redeemed")
    } getOrElse(Map("description" -> "invalid voucher"))
  }

  // match a promotion, now hard coded
  def getPromotion(userId: String, gameId:Int, vGoodId:Int) = {

    queryEvaluator.select("select * from promotions limit 1") { rs =>
      (rs.getInt("id"), rs.getString("description"))
    }.headOption map { case(id, desc) =>
      Map("description" -> desc, "promotionId" -> id)
    } getOrElse(Map("description" -> "no promotion found"))
  }
 
  // the code generated should contain the info of merchant, product_id and amout
  // find a better way to do this
  def createVoucher(userId: String, promotionId:Int) = {
    queryEvaluator.select("select * from promotions where id = ?", promotionId) { rs =>
      (rs.getInt("merchant_id"), rs.getInt("product_id"), rs.getInt("product_amount")) 
    }.headOption map { case(m_id, p_id, amt) =>
      val code = "%s+%d+%d+%d" format(rand nextString(10), m_id, p_id, amt)
                                      
      queryEvaluator.execute("insert into vouchers (user_id, promotion_id, code) values (?, ?, ?)",
        userId, promotionId, code)
      Map("description" -> "voucher created", 
          "code" -> code)
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
    queryEvaluator.execute("update inventory set amount = amount - ? " +
                           "where user_id = ? and game_id = ? and product_id = ?",
                           amount, userId, gameId, vGoodId)
  }

}
