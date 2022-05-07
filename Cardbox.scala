/** File "Cardbox.scala" by KWR for CSE250, Spring 2022.
    Requires ISR.scala plus implementation class compiled in same folder.
    Simple "flat-file database" which goes part-way to implementing a Buffer.
    Top level of "implementation sandwich" with the ISR trait at bottom.
    Can change the implementation just by changing the next line of code.
    CHANGED 3/17/22 to pass in a key-match function rather than use a trait.
 */

//class Cardbox[A](keyComp: (A,A) => Int) extends SortedSLL[A](keyComp)
//class Cardbox[A](keyComp: (A,A) => Int) extends SortedDLL[A](keyComp)
//class Cardbox[A](keyComp: (A,A) => Int) extends SortedArrayISR[A](keyComp)
//class Cardbox[A](keyComp: (A,A) => Int) extends AIOLISLL[A](keyComp)
//class Cardbox[A](keyComp: (A,A) => Int) extends AIOLI[A](keyComp)
//class Cardbox[A](keyComp: (A,A) => Int) extends BALBOA[A](keyComp)
//class Cardbox[A](keyComp: (A,A) => Int) extends BALBOADLL[A](keyComp)
//class Cardbox[A](keyComp: (A,A) => Int) extends BSTISR(keyComp)
//class Cardbox[A](m: Int, hashFun:A=>Int, itemMatch: (A,A) => Boolean) extends HashISR(m,hashFun,itemMatch)
