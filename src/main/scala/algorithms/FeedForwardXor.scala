package algorithms


object FeedForwardXor {
  // train for  { a, b } in { 0, 1 }

  // NeuralNod - *
  /*
   *       L1 -> L2 \
   * xor <     X      > - f
   *       L1 -> L2 /
   **/

  val trainedXor: Boolean ⇒ Boolean ⇒ Int = a ⇒ b ⇒ {
    a ^ b
  }
  
}
