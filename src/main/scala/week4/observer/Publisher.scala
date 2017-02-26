package week4.observer


trait Subscriber {
  def handler(publisher: Publisher): Unit
}

trait Publisher {

  private var subscribers: Set[Subscriber] = Set.empty

  def subscribe(subscriber: Subscriber): Unit =
    subscribers += subscriber

  def unsubscribe(subscriber: Subscriber): Unit =
    subscribers -= subscriber

  def publish(): Unit =
    subscribers foreach (_.handler(this))
}