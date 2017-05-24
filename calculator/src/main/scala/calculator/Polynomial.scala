package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal{
      val bVal = b()
      bVal*bVal-4*a()*c()
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val deltaVal = Signal(delta())
    val bVal = Signal(-1*b())
    val denomVal = Signal(2*a())
    val sqrtDelta = Signal(math.sqrt(deltaVal()))
    Signal {
      if (deltaVal() >= 0) Set(((bVal() + sqrtDelta())/denomVal()), ((bVal() - sqrtDelta()) / denomVal()))
      else if (deltaVal() == 0) Set(bVal() / denomVal())
      else Set()
    }
  }
}
