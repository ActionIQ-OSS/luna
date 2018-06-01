package co.actioniq.luna.compiled

trait BoundedSeq[T] {
  def seq: Seq[T]
  def limit: Int
  require(seq.size <= limit, s"Maximum sequence size is $limit")
}

object BoundedSeq {
  def apply[T](seq: Seq[T]): BoundedSeq[T] = {
    if (seq.size <= 100){
      BoundedSeq100(seq)
    } else if (seq.size <= 200) {
      BoundedSeq200(seq)
    } else if (seq.size <= 500) {
      BoundedSeq500(seq)
    } else if (seq.size <= 1000) {
      BoundedSeq1000(seq)
    } else {
      throw new Exception(s"Invalid sequence length: ${seq.size}")
    }
  }
}

case class BoundedSeq500[T](
  override val seq: Seq[T],
  override val limit: Int = 500
) extends BoundedSeq[T]

case class BoundedSeq1000[T](
  override val seq: Seq[T],
  override val limit: Int = 1000
) extends BoundedSeq[T]

case class BoundedSeq200[T](
  override val seq: Seq[T],
  override val limit: Int = 200
) extends BoundedSeq[T]


case class BoundedSeq100[T](
  override val seq: Seq[T],
  override val limit: Int = 100
) extends BoundedSeq[T]
