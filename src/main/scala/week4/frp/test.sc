import week4.frp.{BankAccount, Signal, Var}

object test {

  def consolidated(accts: List[BankAccount]): Signal[Int] = {
    Signal(accts.map(_.balance()).sum)
  }

  val a = new BankAccount
  val b = new BankAccount
  val c = consolidated(List(a, b))

  c()

  a deposit 20

  c()

  val v = Var(0)
  val x = Signal(v())
  x()
  v() = 2
  x()

}