
//------------------------ LINEAR -------------------------->
class LinearEase (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = (c*t/d+b)
}
//------------------------- QUAD --------------------------->
class QuadEaseIn (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = (c*Math.pow((t/d),2.0)+b)
}
class QuadEaseOut (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = (-c*(t/d)*((t/d)-2.0)+b)
}
class QuadEaseInOut (b:Double, c:Double, d:Double) {
  private def evaluate (t:Double) = 
    if (t<1.0) (c/2.0*Math.pow(t,2.0)+b)
    else (-c/2.0*((t-1.0)*(t-3.0)-1.0)+b)
  def Eval (t:Double) = evaluate(t/d*2.0)
}
class QuadEaseOutIn (b:Double, c:Double, d:Double) {
  val qO = new QuadEaseOut(b,(c/2.0),d)
  val qI = new QuadEaseIn((b+c/2.0),(c/2.0),d)
  def Eval (t:Double) = 
    if (t<(d/2.0)) qO.Eval(t*2.0)
    else qI.Eval((t*2.0)-d)
}
//------------------------- CUBIC -------------------------->
class CubicEaseIn (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = (c*Math.pow((t/d),3.0)+b)
}
class CubicEaseOut (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = (c*(Math.pow((t/d-1.0),3.0)+1.0)+b)
}
class CubicEaseInOut (b:Double, c:Double, d:Double) {
  private def evaluate (t:Double) = 
    if (t<1.0) (c/2.0*t*t*t+b)
    else (-c/2.0*((t-2.0)*(t-2.0)*(t-2.0)+2.0)+b)
  def Eval (t:Double) = evaluate(t/d*2.0)
}
class CubicEaseOutIn (b:Double, c:Double, d:Double) {
  val cO = new CubicEaseOut(b,(c/2.0),d)
  val cI = new CubicEaseIn((b+c/2.0),(c/2.0),d)
  def Eval (t:Double) = 
    if (t<(d/2.0)) cO.Eval(t*2.0)
    else cI.Eval((t*2.0)-d)
}
//------------------------- QUART -------------------------->
class QuartEaseIn (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = (c*Math.pow((t/d),4.0)+b)
}
class QuartEaseOut (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = (-c*(Math.pow((t/d-1.0),4.0)-1.0)+b)
}
class QuartEaseInOut (b:Double, c:Double, d:Double) {
  private def evaluate (t:Double) = 
    if (t<1.0) (c*Math.pow(t,4.0)+b)
    else (-c/2.0*(Math.pow((t-2.0),4.0)-2.0)+b)
  def Eval (t:Double) = evaluate(t/d*2.0)
}
class QuartEaseOutIn (b:Double, c:Double, d:Double) {
  val qO = new QuartEaseOut(b,(c/2.0),d)
  val qI = new QuartEaseIn((b+c/2.0),(c/2.0),d)
  def Eval (t:Double) = 
    if (t<(d/2.0)) qO.Eval(t*2.0)
    else qI.Eval((t*2.0)-d)
}
//------------------------- QUINT -------------------------->
class QuintEaseIn (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = (c*Math.pow((t/d),5.0)+b)
}
class QuintEaseOut (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = (c*(Math.pow((t/d-1.0),5.0)+1.0)+b)
}
class QuintEaseInOut (b:Double, c:Double, d:Double) {
  private def evaluate (t:Double) = 
    if (t<1.0) (c*Math.pow(t,5.0)+b)
    else (-c/2.0*(Math.pow((t-2.0),5.0)+2.0)+b)
  def Eval (t:Double) = evaluate(t/d*2.0)
}
class QuintEaseOutIn (b:Double, c:Double, d:Double) {
  val qO = new QuintEaseOut(b,(c/2.0),d)
  val qI = new QuintEaseIn((b+c/2.0),(c/2.0),d)
  def Eval (t:Double) = 
    if (t<(d/2.0)) qO.Eval(t*2.0)
    else qI.Eval((t*2.0)-d)
}
//-------------------------- SINE -------------------------->
class SineEaseIn (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = (-c*Math.cos(t/d*(Math.PI/2.0))+c+b)
}
class SineEaseOut (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = (c*Math.sin(t/d*(Math.PI/2.0))+b)
}
class SineEaseInOut (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = (-c/2.0*(Math.cos(Math.PI*t/d)-1.0)+b)
}
class SineEaseOutIn (b:Double, c:Double, d:Double) {
  val sO = new SineEaseOut(b,(c/2.0),d)
  val sI = new SineEaseIn((b+c/2.0),(c/2.0),d)
  def Eval (t:Double) = 
    if (t<(d/2.0)) sO.Eval(t*2.0)
    else sI.Eval((t*2.0)-d)
}
//-------------------------- EXPO -------------------------->
class ExpoEaseOut (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = 
    if (t.equals(0)) (b+c)
    else (c*1.001*(-Math.pow(2.0,(-10.0*t/d))+1.0)+b)
}
class ExpoEaseIn (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = 
    if (t.equals(d)) b
    else (c*Math.pow(2.0,(10.0*(t/d-1.0)))+b-c*0.001)
}
class ExpoEaseInOut (b:Double, c:Double, d:Double) {
  def Eval (t:Double) =
    if (t.equals(0)){b}
    else if (t.equals(d)){(b+c)}
    else{
      var t2 = (t/d*2.0)
      if (t2<1.0){
        (c/2.0*Math.pow(2.0,(10.0*(t2-1.0)))+b-c*0.0005)
      }else{
        t2 = (t2-1.0)
        (c/2.0*1.0005*(-Math.pow(2.0,(-10.0*t2))+2.0)+b)
}}}
class ExpoEaseOutIn (b:Double, c:Double, d:Double) {
  val eO = new ExpoEaseOut(b, (c/2.0), d)
  val eI = new ExpoEaseIn((b+c/2.0), (c/2.0), d)
  def Eval (t:Double) = 
    if (t<(d/2.0)) eO.Eval(t*2.0)
    else eI.Eval((t*2.0)-d)
}
//------------------------- CIRC --------------------------->
class CircEaseIn (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = 
    (-c*(Math.sqrt(1.0-Math.pow((t/d),2.0))-1.0)+b)
}
class CircEaseOut (b:Double, c:Double, d:Double) {
  def Eval (t:Double) = 
    (c*Math.sqrt(1.0-Math.pow((t/d-1.0),2.0))+b)
}
class CircEaseInOut (b:Double, c:Double, d:Double) {
  private def evaluate (t:Double) = 
    if (t<1.0){(-c/2.0*(Math.sqrt(1.0-t*t)-1.0)+b)}
    else{
     val t2 = (t-2.0)
     (c/2.0*(Math.sqrt(1.0-t2*t2)-1.0)+b)
    }
  def Eval (t:Double) = evaluate(t/d*2.0)
}
class CircEaseOutIn (b:Double, c:Double, d:Double) {
  val cO = new CircEaseOut(b,(c/2.0),d)
  val cI = new CircEaseIn((b+c/2.0),(c/2.0),d)
  def Eval (t:Double) = 
    if (t<(d/2.0)) cO.Eval(t*2.0)
    else cI.Eval((t*2.0)-d)
}
//------------------------- BACK --------------------------->
class BackEaseIn (b:Double, c:Double, d:Double, s:Double) {
  def Eval (t:Double) = { 
    val t2 = (t/d)
    (c*t2*t2*((s+1.0)*t2-s)+b)
}}
class BackEaseOut (b:Double, c:Double, d:Double, s:Double) {
  def Eval (t:Double) = {
    val t2 = (t/d)
    (c*(t2*t2*((s+1.0)*t2+s)+1.0)+b)
}}
class BackEaseInOut (b:Double, c:Double, d:Double, s:Double) {
  val s2 = (s*1.525)
  private def evaluate (t:Double) = 
    if (t<1.0)(c/2.0*(t*t*((s+1.0)*t-s))+b)
    else{
      val t2 = (t-2.0)
      (c/2.0*(t*t*((s+1.0)*t+s)+2.0)+b)
    }
  def Eval (t:Double) = evaluate(t/d*2.0)
}
class BackEaseOutIn (b:Double, c:Double, d:Double) {
  val bO = new QuartEaseOut(b,(c/2.0),d)
  val bI = new QuartEaseIn((b+c/2.0),(c/2.0),d)
  def Eval (t:Double) = 
    if (t<(d/2.0)) bO.Eval(t*2.0)
    else bI.Eval((t*2.0)-d)
}
//------------------------- BOUNCE ------------------------->
class BounceEaseOut (b:Double, c:Double, d:Double) {
  private def evaluate (t:Double) = {
    if (t<(1.0/2.75)){
      (c*(7.5625*t*t)+b)
    }
    else if (t<(2.0/2.75)){
      val t2 = (t-(1.5/2.75))
      (c*(7.5625*t*t+0.75)+b)
    }
    else if (t<(2.5/2.75)){
      val t2 = (t-(2.5/2.75))
      (c*(7.5625*t*t+0.9375)+b)
    }else{
      val t2 = (t-(2.625/2.75))
      (c*(7.5625*t*t+0.984375)+b)
    }}  
  def Eval (t:Double) = evaluate(t/d) 
}
class BounceEaseIn (b:Double, c:Double, d:Double) {
  val bO = new BounceEaseOut(0.0,c,d)
  def Eval (t:Double) = (c-bO.Eval(d-t)+b)
}
class BounceEaseInOut (b:Double, c:Double, d:Double) {
  val bI = new BounceEaseIn(0.0,c,d)
  val bO = new BounceEaseOut(0.0,c,d)
  private def evaluate (t:Double) = 
    if (t<(d/2.0))(bI.Eval(t*2.0)*0.5+b)
    else (bO.Eval(t*2.0-d)*0.5+c*0.5+b)
  def Eval (t:Double) = evaluate(t/d*2.0)
}
class BounceEaseOutIn (b:Double, c:Double, d:Double) {
  val bO = new BounceEaseOut(b,(c/2.0),d)
  val bI = new BounceEaseIn((b+c/2.0),(c/2.0),d)
  def Eval (t:Double) = 
    if (t<(d/2.0)) bO.Eval(t*2.0)
    else bI.Eval((t*2.0)-d)
}
