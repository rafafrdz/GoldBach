![](https://raw.githubusercontent.com/rafafrdz/GoldBach/master/goldbach.png)

```scala
object Goldbach extends App {
  import inform.graphics._
  import scala.collection.mutable.SortedSet
  import scala.collection.mutable.BitSet
  import scala.math._
  def paresHasta(n:Int):SortedSet[Int] = {
    val c = SortedSet[Int]()
    for(i<-4 to n by 2)c+=i
    c
  }
  def raizCuadrada(x:Int):Int = sqrt(x.toDouble).toInt
  def criba(n:Int):BitSet = {
    val p = BitSet()
    for(i<-2 to n) p+=i
    val k = raizCuadrada(n)
    for(j<-2 to k){
      if(p.contains(j)){
        for(h<-2 to n){
          var m = j*h
          p-=m
        }
      }
    }
    p
  }
  def sumas(n:Int):Int = {
    val c = criba(n)
    var ac = 0
    val b = BitSet()
    for(i<-c){
      if(c.contains(n-i) && !b.contains(i)){
        b+=(n-i)
        ac+=1
      }
    }
    ac
  }
  def GoldBach(n:Int) {
    val serie=XYSeries("Número de parejas de primos distintos que suman n")
    for(i<-paresHasta(n)) serie+=(i,sumas(i))
    val seriesColl = XYSeriesCollection()
    seriesColl+=serie
    val chart = XYBarChart("Conjetura de Goldbach","X","Numero de parejas",seriesColl)
    chart.draw(1000,600)
  }
  println(GoldBach(100))
}
```

