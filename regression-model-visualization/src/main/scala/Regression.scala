import scala.math.log


/**
  * This is the base class for the different regression models. It offers some functionality
  *  for all the different regression models (Linear, Exponential, Logarithmic). Regression models are constructed
  *  using least squares method. See: http://mathworld.wolfram.com/LeastSquaresFitting.html
  */
abstract class Regression(var seq: Seq[(Double, Double)], title: String) {

  /**This simple function returns the title of the regression model. The title is created when the user imports a data set
    * into the program. The program sets the file name as the value of the title. This value is used to check that no same file
    * is imported two times
    */
  def getTitle: String = this.title

  /**
    This method changes the sequence for the regression model. If the range of the data points is too small or too big, we map
    them to a bigger or smaller range respectively.
    */
  def changeSeq(sequence: Seq[(Double,Double)]) = {
    seq = sequence
  }


  /**This function creates the needed parameters for the fitted curve. It is different for all the regression models.*/
  def regression(): Option[(Double, Double)]


  /**This function generates a tuple where the values are the sums of the data points.*/
  def sums(sequence: Seq[(Double, Double)]): (Double, Double) = {
    var countX = 0.0
    var countY = 0.0
    for (param <- sequence) {
      countX += param._1
      countY += param._2
    }
    (countX, countY)
  }
  /**This function sums the products of each tuple.*/
  def sumXY(sequence: Seq[(Double, Double)]): Double = {

    var countMul = 0.0

    for(param <- sequence) {
      countMul += param._1*param._2
    }
    countMul
  }
  /**This function sums the squares of the x-values*/
  def sumXsquared(sequence: Seq[(Double, Double)]): Double = {
    var squared = 0.0

    for(param <- sequence) {
      squared += param._1*param._1
    }
    squared
  }
}

/**This class is for the linear model. The web-page https://brilliant.org/wiki/linear-regressio/ presents the functionality
  * of this method.
  */
case class Linear(seqLinear: Seq[(Double, Double)], title: String) extends Regression(seqLinear,title) {

  /**Returns the constants for linear regression y = k*x+b*/
  def regression(): Option[(Double, Double)] = {

    /**The numerator and the denominator of the slope*/
    val slopeNum: Double = seqLinear.length*this.sumXY(seqLinear) - this.sums(seqLinear)._1*this.sums(seqLinear)._2
    val slopeDen: Double = seqLinear.length*this.sumXsquared(seqLinear)-this.sums(seqLinear)._1*this.sums(seqLinear)._1

    /**The numerator and the denominator of the constant*/
    val constantNum: Double = this.sumXsquared(seqLinear)*this.sums(seqLinear)._2-this.sums(seqLinear)._1*this.sumXY(seqLinear)
    val constantDen: Double = seqLinear.length*this.sumXsquared(seqLinear)-this.sums(seqLinear)._1*this.sums(seqLinear)._1

    Some(slopeNum/slopeDen, constantNum/constantDen)
  }


}
/**This regression model is for the exponential functions of form y=a*e(bx). See: http://mathworld.wolfram.com/LeastSquaresFittingExponential.html*/
case class Exponential(seqExp: Seq[(Double, Double)], title: String) extends Regression(seqExp, title) {

  /** Creating new sequences to utilize some functionalities from the Regression class. */
  val xLogyLog = seqExp.map { case (x1: Double, x2: Double) => (log(x1), log(x2)) }
  val xSquaredy = seqExp.map { case (x1: Double, x2: Double) => (x1 * x1, x2) }
  val yLogy = seqExp.map { case (x1: Double, x2: Double) => (x1, x2 * log(x2)) }

  /**Generates the constants for exponential regression y = b*e pow (kx)*/
  def regression(): Option[(Double, Double)] = {

    /** The numerator and the denominator of the slope */
    val slopeNum: Double = this.sums(seqExp)._2 * this.sumXY(yLogy) - this.sumXY(seqExp) * this.sums(yLogy)._2
    val slopeDen: Double = this.sums(seqExp)._2 * this.sumXY(xSquaredy) - this.sumXY(seqExp) * this.sumXY(seqExp)

    /** The numerator and the denominator of the constant */
    val constantNum: Double = this.sums(yLogy)._2 * this.sumXY(xSquaredy) - this.sumXY(seqExp) * this.sumXY(yLogy)
    val constantDen: Double = this.sums(seqExp)._2 * this.sumXY(xSquaredy) - this.sumXY(seqExp) * this.sumXY(seqExp)
    Some(slopeNum / slopeDen, constantNum / constantDen)

  }

}
  /**This regression model is for the logarithmic functions of form y=a+b*ln(x). See: http://mathworld.wolfram.com/LeastSquaresFittingLogarithmic.html*/
  case class Logarithmic(seqLog: Seq[(Double, Double)], title: String) extends Regression(seqLog, title) {

    /**Creating new sequence to utilize some functionalities from the Regression class.*/
    val logX = seqLog.map {case (x1: Double, x2: Double) => (log(x1), x2) }

    /**Returns the regression model for logarithmic functions y = b + k*ln(x)*/
    def regression(): Option[(Double, Double)] = {

      /**The numerator and the denominator of the slope*/
      val slopeNum: Double = seqLog.length*this.sumXY(logX)-this.sums(logX)._1*this.sums(logX)._2
      val slopeDen: Double = seqLog.length*this.sumXsquared(logX)-this.sums(logX)._1*this.sums(logX)._1

      /**The numerator and the denominator of the constant*/
      val constantNum: Double = this.sums(logX)._2-slopeNum/slopeDen*this.sums(logX)._1
      val constantDen: Double = seqLog.length

      Some(slopeNum/slopeDen, constantNum/constantDen)
    }

}
