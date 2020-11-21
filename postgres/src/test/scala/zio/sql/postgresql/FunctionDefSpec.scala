package zio.sql.postgresql

import zio.Cause
import zio.test._
import zio.test.Assertion._

object FunctionDefSpec extends PostgresRunnableSpec with ShopSchema {

  import this.Customers._
  import this.PostgresFunctionDef._
  import this.FunctionDef._

  val spec = suite("Postgres FunctionDef")(
    testM("sin") {
      val query = select(Sin(1.0)) from customers

      val expected = 0.8414709848078965

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("sind") {
      val query = select(Sind(30.0)) from customers

      val expected = 0.5

      val testResult = execute(query).to[Double, Double](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("quote_nullable") {
      val query = select(QuoteNullable(30.0)) from customers

      val expected = "'30.0'"

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("quote_nullable with None") {
      val query = select(QuoteNullable(None.asInstanceOf[Option[Double]])) from customers

      val expected = "'30.0'"

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    },
    testM("quote_nullable with Some") {
      val a: Option[Double] = Some(30.0)
      val query = select(QuoteNullable(a)) from customers

      val expected = "'30.0'"

      val testResult = execute(query).to[String, String](identity)

      val assertion = for {
        r <- testResult.runCollect
      } yield assert(r.head)(equalTo(expected))

      assertion.mapErrorCause(cause => Cause.stackless(cause.untraced))
    }
  )
}
