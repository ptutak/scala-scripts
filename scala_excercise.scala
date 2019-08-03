import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.collection.mutable.ListBuffer
import util.control.Breaks._

class Node(val newValue: Int) {
    var value = newValue
    var left: Option[Node] = None
    var right: Option[Node] = None
    var parent: Option[Node] = None
}

class Tree {
    var leftWeight = 0
    var rightWeight = 0
    var root: Option[Node] = None

    def insertNode(node: Option[Node]): Unit = {
        var parent: Option[Node] = None
        var tmp: Option[Node] = root
        val value: Int = node.get.value

        if (root.isDefined) {
            if (value < root.get.value)
                leftWeight += 1
            else
                rightWeight += 1
        }
        while (tmp.isDefined) {
            parent = tmp
            if (value < tmp.get.value)
                tmp = tmp.get.left
            else
                tmp = tmp.get.right
        }
        node.get.parent = parent
        if (parent.isEmpty) {
            root = node
        } else {
            if (value < parent.get.value)
                parent.get.left = node
            else
                parent.get.right = node
        }
        ()
    }

    def findValue(rootNode: Option[Node] ,value: Int): Option[Node] = {
        var side: Option[String] = None
        var root = rootNode
        while(root.isDefined && (root.get.value != value)) {
            if (value < root.get.value)
                root = root.get.left
            else
                root = root.get.right
        }
        return root
    }

    def findMin(rootNode: Option[Node]): Option[Node] = {
        var node = rootNode
        while(node.get.left.isDefined)
            node = node.get.left
        return node
    }

    def findSuccessor(rootNode: Option[Node]): Option[Node] = {
        var node = rootNode
        if (node.get.right.isDefined) {
            return findMin(node.get.right)
        }
        var tmpNode: Option[Node] = node.get.parent
        while (tmpNode.isDefined && tmpNode.get.left != node) {
            node = tmpNode
            tmpNode = tmpNode.get.parent
        }
        return tmpNode
    }

    def findMax(rootNode: Option[Node]): Option[Node] = {
        var node = rootNode
        while(node.get.right.isDefined)
            node = node.get.right
        return node
    }

    def findPredecessor(rootNode: Option[Node]): Option[Node] = {
        var node = rootNode
        if (node.get.left.isDefined)
            return findMax(node.get.left)
        var tmpNode: Option[Node] = node.get.parent
        while (tmpNode.isDefined && (tmpNode.get.right != node)){
            node = tmpNode
            tmpNode = tmpNode.get.parent
        }
        return tmpNode
    }

    def deleteNode(node: Option[Node]): Option[Node] = {
        var successor: Option[Node] = None
        if (node.get.value < root.get.value)
            leftWeight -= 1
        else if ((node == root) && (root.get.right.isDefined))
            rightWeight -= 1
        else if (node.get.value >= root.get.value)
            rightWeight -= 1
        else
            leftWeight -= 1


        if (node.get.left.isEmpty || node.get.right.isEmpty)
            successor = node
        else
            successor = findSuccessor(node)

        var successorChild: Option[Node] = None
        if (successor.get.left.isDefined)
            successorChild = successor.get.left
        else
            successorChild = successor.get.right

        if (successorChild.isDefined)
            successorChild.get.parent = successor.get.parent

        if (successor.get.parent.isEmpty)
            this.root = successorChild
        else {
            if (successor == successor.get.parent.get.left)
                successor.get.parent.get.left = successorChild
            else
                successor.get.parent.get.right = successorChild
        }
        if (successor != node){
            node.get.value = successor.get.value
        }

        return successor
    }

    def rotateRight: Unit = {
        val predecessor = findPredecessor(root)
        if (predecessor.isDefined && predecessor != root) {
            val tmpRoot: Int = root.get.value
            val tmpPredecessor: Int = predecessor.get.value
            deleteNode(predecessor)
            root.get.value = tmpPredecessor
            addValue(tmpRoot)
        }
    }

    def rotateLeft: Unit = {
        val successor = findSuccessor(root)
        if (successor.isDefined && successor != root) {
            if (successor.get.value == root.get.value) {
                val tmpRoot = root.get.value
                deleteNode(root)
                val tmpLeftRoot = root.get.left
                root.get.left = new Some(new Node(tmpRoot))
                insertNode(tmpLeftRoot)
            } else {
                val tmpRoot: Int = root.get.value
                val tmpSuccessor: Int = successor.get.value
                deleteNode(successor)
                root.get.value = tmpSuccessor
                addValue(tmpRoot)
            }
        }
    }

    def balanceTree: Unit = {
        while (leftWeight - rightWeight > 1){
            rotateRight
        }
        while (rightWeight - leftWeight > 0){
            rotateLeft
        }
    }

    def addValue(value: Int): Unit = {
        val node: Option[Node] = new Some(new Node(value))
        insertNode(node)
        ()
    }

    def addValuesBalance(arr: Array[Int]): Unit = {
        arr.foreach(addValue)
        balanceTree
    }
}

object Solution {

     // Complete the activityNotifications function below.
    def activityNotifications(expenditure: Array[Int], d: Int): Int = {
        var notifs: Int = 0
        val expSlice = expenditure.slice(0, d)
        var myTree = new Tree
        myTree.addValuesBalance(expSlice)
        var j = 0
        for (i <- Array.range(d, expenditure.length)) {
            var median = 0.0
            if (d % 2 == 1)
                median = myTree.root.get.value
            else
                median = (myTree.root.get.value + myTree.findPredecessor(myTree.root).get.value) / 2.0

            if (expenditure(i) >= 2 * median)
                notifs += 1
            println(myTree.leftWeight, myTree.rightWeight, myTree.root.get.value)
            println(median, notifs)
            if (i + 1 < expenditure.length) {
                val toRemove = expenditure(j)
                val toInsert = expenditure(i + 1)
                myTree.deleteNode(myTree.findValue(myTree.root, toRemove))
                myTree.addValue(toInsert)
                myTree.balanceTree

                j += 1
            }
        }
        return notifs
    }

    def main(args: Array[String]) {
        /*
        val stdin = scala.io.StdIn

        val nd = stdin.readLine.split(" ")

        val n = nd(0).trim.toInt

        val d = nd(1).trim.toInt

        val expenditure = stdin.readLine.split(" ").map(_.trim.toInt)
        val result = activityNotifications(expenditure, d)
        */
        val expenditure = Array(1, 2, 3, 4, 4)
        val result = activityNotifications(expenditure, 4)
        println(result)
    }
}
