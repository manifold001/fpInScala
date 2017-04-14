package gettingstarted


import org.scalatest.{FunSpec, Matchers}
class GettingStartedSpec extends FunSpec with Matchers {
  describe("GettingStarted") {
    describe("function fib") {
      it("should return 0 if the n is 1") {
        GettingStarted.fib(1) shouldBe 0
      }
      it("should return 1 if the n if 2") {
        GettingStarted.fib(2) shouldBe 1
      }

      it("should meet the condition that fib(n) = fib(n-1) + fib(n-2)") {
        1 to 10 foreach{ n => GettingStarted.fib(n+2) shouldBe (GettingStarted.fib(n + 1) + GettingStarted.fib(n))}
      }
    }
  }
}
