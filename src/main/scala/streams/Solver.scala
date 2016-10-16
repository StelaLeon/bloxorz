package streams

/**
 * This component implements the solver for the Bloxorz game
 */
trait Solver extends GameDef {

  /**
   * Returns `true` if the block `b` is at the final position
   */
  def done(b: Block): Boolean = {
    val res = b.b1.x.equals(goal.x) && b.isStanding && b.b1.x.equals(goal.x) && b.b1.y.equals(goal.y)
    res
  }


  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
    if(!b.isLegal) Stream.empty
    else {
      val neighbors = b.legalNeighbors
      val more = for {
        n <- neighbors
      } yield( n._1,n._2 :: history)
      more.toStream
    }
  }

  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] = {
    neighbors.filter(n=> !explored.contains(n._1))
  }

  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {
    if(initial.isEmpty)
      Stream.empty
    else {
      var newExplored = explored
      val newInitial = initial.flatMap(elem=>{
        if(!explored.contains(elem._1)){
          val streamOfThings = neighborsWithHistory(elem._1,elem._2)
          newExplored ++= Set(elem._1)
          newNeighborsOnly(streamOfThings,explored)
        }
        else{
          Stream.empty
        }
      })
      if(newInitial.isEmpty)
        return Stream.empty
      else
        initial #::: newInitial #::: from(newInitial, newExplored)
    }
  }

  /**
   * The stream of all paths that begin at the starting block.
   */
  lazy val pathsFromStart: Stream[(Block, List[Move])] = {
    val exploredBlocks = Set(startBlock)
    val startingPath = newNeighborsOnly(
      neighborsWithHistory(startBlock,List.empty[Move]),
            exploredBlocks)

    from(startingPath,exploredBlocks)
  }

  /**
   * Returns a stream of all possible pairs of the goal block along
   * with the history how it was reached.
   */
  lazy val pathsToGoal: Stream[(Block, List[Move])] = {
    //println("does it contain the goal? " + pathsFromStart.toString())
    val paths = pathsFromStart.toList.filter(elem=>(done(elem._1)))
    paths.toStream
  }

  /**
   * The (or one of the) shortest sequence(s) of moves to reach the
   * goal. If the goal cannot be reached, the empty list is returned.
   *
   * Note: the `head` element of the returned list should represent
   * the first move that the player should perform from the starting
   * position.
   */
  lazy val solution: List[Move] = {
    val solutions = pathsToGoal.toList.filter(elem => done(elem._1))
    solutions.head._2
  }
}
