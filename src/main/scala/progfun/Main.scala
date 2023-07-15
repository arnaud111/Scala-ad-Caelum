package fr.esgi.al.funprog

import scala.annotation.tailrec

class Lawnmower (
  val start_x: Integer,
  val start_y: Integer,
  val start_dir: Char,
  val dir: Char,
  val pos_x: Integer,
  val pos_y: Integer,
  val instructions: String
) {

  def toJson(): String = {
    "{" +
      "\"debut\": {" +
        "\"point\": {" +
          "\"x\": " + this.start_x.toString + "," +
          "\"y\": " + this.start_y.toString +
        "}," +
        "\"direction\": \"" + this.start_dir.toString + "\"" +
      "}," +
      "\"instructions\": " + this.instructions.toArray.mkString("[\"", "\",\"", "\"],") +
      "\"fin\": {" +
        "\"point\": {" +
          "\"x\": " + this.pos_x.toString + "," +
          "\"y\": " + this.pos_y.toString +
        "}," +
        "\"direction\": \"" + this.dir.toString + "\"" +
      "}" +
    "}"
  }

  def move(i: Integer, max_x: Integer, max_y: Integer): Lawnmower = {
    if (instructions.length > i) {
      (instructions.charAt(i), this.dir) match {
        case ('A', 'N') =>
          if (this.pos_y < max_y) {
            new Lawnmower(this.start_x, this.start_y, this.start_dir, this.dir, this.pos_x, this.pos_y + 1, this.instructions).move(i + 1, max_x, max_y);
          } else {
            this.move(i + 1, max_x, max_y);
          }
        case ('A', 'S') =>
          if (this.pos_y > 0) {
            new Lawnmower(this.start_x, this.start_y, this.start_dir, this.dir, this.pos_x, this.pos_y - 1, this.instructions).move(i + 1, max_x, max_y);
          } else {
            this.move(i + 1, max_x, max_y);
          }
        case ('A', 'E') =>
          if (this.pos_x < max_x) {
            new Lawnmower(this.start_x, this.start_y, this.start_dir, this.dir, this.pos_x + 1, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
          } else {
            this.move(i + 1, max_x, max_y);
          }
        case ('A', 'W') =>
          if (this.pos_x > 0) {
            new Lawnmower(this.start_x, this.start_y, this.start_dir, this.dir, this.pos_x - 1, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
          } else {
            this.move(i + 1, max_x, max_y);
          }
        case ('D', 'N') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'E', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
        case ('D', 'S') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'W', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
        case ('D', 'E') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'S', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
        case ('D', 'W') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'N', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
        case ('G', 'N') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'W', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
        case ('G', 'S') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'E', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
        case ('G', 'E') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'N', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
        case ('G', 'W') =>
          new Lawnmower(this.start_x, this.start_y, this.start_dir, 'S', this.pos_x, this.pos_y, this.instructions).move(i + 1, max_x, max_y);
      }
    } else {
      this
    }
  }
}

object Main extends App {

  val x: Integer = 5;
  val y: Integer = 5;

  val lawnmowers: List[Lawnmower] = List(
    new Lawnmower(1, 2, 'N', 'N', 1, 2, "GAGAGAGAA"),
    new Lawnmower(3, 3, 'N', 'E', 3, 3, "AADAADADDA"),
  );

  val ls = computeLawnmowers(lawnmowers);
  val s =  gardenToJson(ls, x, y);

  println(s);

  def computeLawnmowers(lawnmowers: List[Lawnmower]): List[Lawnmower] = {
    lawnmowers.headOption match {
      case Some(value) =>
        val l = value.move(0, x, y);
        val ls = computeLawnmowers(lawnmowers.drop(1));
        ls.appended(l);
      case None =>
        println("End")
        lawnmowers
    }
  }

  @tailrec
  def displayLawnmowers(lawnmowers: List[Lawnmower]): Unit = {
    lawnmowers.headOption match {
      case Some(value) =>
        println(value.pos_x.toString + " " + value.pos_y.toString + " " + value.instructions);
        displayLawnmowers(lawnmowers.drop(1));
      case None =>
        println("End")
    }
  }

  def gardenToJson(lawnmowers: List[Lawnmower], x: Integer, y: Integer): String = {
    "{" +
      "\"limite\": {" +
        "\"x\": " + x.toString + "," +
        "\"y\": " + y.toString +
      "}," +
      "\"tondeuses\": [" +
        lawnmowers.map(l => l.toJson()).mkString("", ",", "") +
      "]" +
    "}";
  }
}
