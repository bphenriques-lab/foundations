package exercises.errorhandling.project

import exercises.errorhandling.NEL
import exercises.errorhandling.project.OrderError._
import exercises.errorhandling.project.OrderStatus._

import java.time.{Duration, Instant}

case class Order(
  id: OrderId,
  createdAt: Instant,   // set when the order is created ("Draft")
  status: OrderStatus // "Draft", "Checkout", "Submitted" or "Delivered"
) {

  // Adds an `Item` to the basket.
  // This action is only permitted if the `Order` is in "Draft" or "Checkout" statuses.
  // If the `Order` is in "Checkout" status, move it back to "Draft".
  // Note: We don't verify if the `Item` is already in the basket.
  def addItems(items: NEL[Item]): Either[OrderError, Order] =
    status match {
      case d: Draft    => Right(copy(status = d.copy(basket = d.basket ++ items.toList)))
      case c: Checkout => Right(copy(status = c.copy(basket = c.basket ++ items)))
      case _           => Left(InvalidStatus(status))
    }

  // 1. Implement `checkout` which attempts to move the `Order` to "Checkout" status.
  // `checkout` requires the order to be in the "Draft" status, otherwise it returns an `InvalidStatus` error.
  // `checkout` requires the order to contain at least one item, otherwise it returns an `EmptyBasket` error.
  def checkout: Either[OrderError, Order] = status match {
    case Draft(basket) =>
      if (basket.isEmpty) Left(EmptyBasket)
      else Right(copy(status = Checkout(NEL.fromList(basket).get, deliveryAddress = None)))
    case _ => Left(InvalidStatus(status))
  }

  def updateDeliveryAddress(address: Address): Either[OrderError, Order] =
    status match {
      case c @ Checkout(_, _) => Right(copy(status = c.copy(deliveryAddress = Some(address))))
      case _                                => Left(InvalidStatus(status))
    }

  // 2. Implement `submit` which attempts to move the `Order` to "Submitted" status.
  // `submit` requires the order to be in the "Checkout" status and to have a delivery address.
  // If `submit` succeeds, the resulting order must be in "Submitted" status and
  // have the field `submittedAt` defined.
  // Note: You may need to extend `OrderError`
  def submit(now: Instant): Either[OrderError, Order] =
    status match {
      case Checkout(basket, deliveryAddress) =>
        deliveryAddress match {
          case Some(address) => Right(copy(status = Submitted(basket, address, now)))
          case None => Left(OrderError.InvalidStatus(status))
        }
      case _ => Left(InvalidStatus(status))
    }

  // 3. Implement `deliver` which attempts to move the `Order` to "Delivered" status.
  // `deliver` requires the order to be in the "Submitted" status.
  // If `deliver` succeeds, the resulting order must be in "Delivered" status and
  // have the field `deliveredAt` defined.
  // If `deliver` succeeds, it also returns the time it took to deliver the order (duration
  // between `submittedAt` and `deliveredAt`).
  // Note: You may need to extend `OrderError`
  def deliver(now: Instant): Either[OrderError, Order] =
    status match {
      case Submitted(basket, deliveryAddress, submittedAt) => Right(copy(status = Delivered(basket, deliveryAddress, submittedAt, now)))
      case _ => Left(OrderError.InvalidStatus(status))
    }
}

object Order {
  // Creates an empty draft order.
  def empty(id: OrderId, now: Instant): Order =
    Order(
      id = id,
      createdAt = now,
      Draft(Nil)
    )
}

case class OrderId(value: String)
case class ItemId(value: String)
case class Item(id: ItemId, quantity: Long, price: BigDecimal)
case class Address(streetNumber: Int, postCode: String)

