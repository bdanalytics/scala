package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("Block methods") {
    new Level1 {
      assert(Block(Pos(1, 1), Pos(1, 1)).isStanding)
      assert(!Block(Pos(1, 1), Pos(1, 2)).isStanding)
      assert(!Block(Pos(1, 1), Pos(2, 1)).isStanding)

      assert(Block(Pos(1, 1), Pos(1, 1)).isLegal)
      assert(!Block(Pos(0, 3), Pos(0, 3)).isLegal)

      assert(!Block(Pos(0, 2), Pos(0, 3)).isLegal)
      assert(!Block(Pos(0, 3), Pos(0, 4)).isLegal)
      assert(!Block(Pos(0, 3), Pos(1, 3)).isLegal)

      assert(Block(Pos(1,1), Pos(1,1)).neighbors.toSet === List(
        (Block(Pos(1,-1),Pos(1,0)),Left),
        (Block(Pos(1,2),Pos(1,3)),Right),
        (Block(Pos(-1,1),Pos(0,1)),Up),
        (Block(Pos(2,1),Pos(3,1)),Down)).toSet)

      assert(Block(Pos(1,1), Pos(1,2)).neighbors.toSet === List(
        (Block(Pos(1,0),Pos(1,0)),Left),
        (Block(Pos(1,3),Pos(1,3)),Right),
        (Block(Pos(0,1),Pos(0,2)),Up),
        (Block(Pos(2,1),Pos(2,2)),Down)).toSet)

      assert(Block(Pos(1,1), Pos(1,1)).legalNeighbors.toSet === List(
        //        (Block(Pos(1,-1),Pos(1,0)),Left),
        (Block(Pos(1,2),Pos(1,3)),Right),
        //        (Block(Pos(-1,1),Pos(0,1)),Up),
        (Block(Pos(2,1),Pos(3,1)),Down)
      ).toSet)

      assert(Block(Pos(1,1), Pos(1,2)).legalNeighbors.toSet === List(
        (Block(Pos(1,0),Pos(1,0)),Left),
        (Block(Pos(1,3),Pos(1,3)),Right),
        (Block(Pos(0,1),Pos(0,2)),Up),
        (Block(Pos(2,1),Pos(2,2)),Down)).toSet)

    }
  }

  test("solver methods") {
    new Level1 {
      assert(!done(Block(Pos(1, 1), Pos(1, 1))))
      assert(done(Block(Pos(4, 7), Pos(4, 7))))

      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List()) ===
        Stream((Block(Pos(1, 2), Pos(1, 3)), List(Right)), (Block(Pos(2, 1), Pos(3, 1)), List(Down))))

      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet ===
        Set((Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))))

      assert(newNeighborsOnly(
        Set(
          (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream,
        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      ) ===
        Set(
          (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
        ).toStream)

      val chkFromRes = from(
        Set(
          (Block(Pos(1,1),Pos(1,1)), List())
        ).toStream,
        Set(Block(Pos(1,1),Pos(1,1)))
      )
      val goalElems = chkFromRes filter (e => e._1 == Block(Pos(4,7),Pos(4,7)))
      println("level 1 goalElems.toList: " + goalElems.toList)
      assert(goalElems.toList.length === 3)

    }
  }

  test("optimal solution for level 1") {
    new Level1 {
//      override lazy val solution = pathsToGoal.head._2
      println("Level1: solution: " + solution)
      println("Level1: goal: " + goal)
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
//      override lazy val solution = List(Down, Right, Right, Right, Down, Right, Right)
      assert(solution.length == optsolution.length)
    }
  }

}
