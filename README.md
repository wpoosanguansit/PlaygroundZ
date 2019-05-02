# Problem

You must write a program for a robot that navigates a closed maze in search for letters that were printed on the
floor of the maze. The solution consists of the list of all letters, ordered alphabetically. The letters are
capitals (A-Z) and the same letter may exist in multiple locations.

Example

The solution for the following maze is AABQ:

\#############  
\#   A&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;#   #  
\# ## ## # # &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;#  
\# Q# #   B  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;#  
\#  #A# # &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;####  
\#############  

\# symbols indicate walls, which are illegal positions. Your robot can navigate on all other cells vertically or
horizontally but not diagonally. You can see the 4 neighbors cells (up, down, left, right) around your current cell.

The initial position of the robot can be any non-wall cell (empty or letter), inside the maze.

Navigation API

Your program will read cell information from stdin, then write a command on stdout, and loop.

Cell information is given as one line of 5 characters indicating the type of the current cell and the type of each
of the neighbor cells in the following order: current, up, down, left, right. In the example, if the current position
is the position of the letter Q, cell information will be given as:

Q#  #

A command is either one of the 4 navigation commands indicating which neighbor cell to move to
(U: up, D: down, L: left, R: right) or the key for escaping the maze starting with the K prefix, e.g. KAABQ which
terminates the game. Commands are terminated by a newline character (LF).

An attempt to move into a wall or giving a wrong key terminates the life of the poor robot.

Here is a very simple maze:

\####  
\#AB#  
\####  

A successful session for this maze, assuming a start position on cell marked with A is:

 input: A###B
output: R
 input: B##A#
output: KAB

# Installation

1. Install Java  
https://www.digitalocean.com/community/tutorials/how-to-install-java-on-ubuntu-with-apt-get  
2. Install Scala and sbt
  
on Mac OS: 

http://biercoff.com/how-to-install-scala-on-mac-os/

on Ubuntu:
  
\#!/bin/sh  
\# one way (older scala version will be installed)  
\# sudo apt-get install scala  
\#2nd way  
sudo apt-get remove scala-library scala  
wget http://www.scala-lang.org/files/archive/scala-2.11.4.deb  
sudo dpkg -i scala-2.11.4.deb  
sudo apt-get update  
sudo apt-get install scala  
\# sbt installation  
\# remove sbt:>  sudo apt-get purge sbt.  
wget http://dl.bintray.com/sbt/debian/sbt-0.13.6.deb  
sudo dpkg -i sbt-0.13.6.deb  
sudo apt-get update  
sudo apt-get install sbt  
  
3. Build the project  
  
Go to the project directory in the console and issue the command 

> sbt assembly  

# To run the program

1. Run by using jar file  
  
1.1 create the jar with sbt assembly - sbt assembly in sbt console  
2. Run the jar file by:  
  
java -jar target/scala-2.11/solution.jar  
  
2.1 Run in the console  
  
Move to the project in the console.  And issue command  
  
> sbt console  
  
Once in the console, you should see:  
  
[info]  
import scalaz.zio._  
import cats.implicits._  
import eu.timepit.refined._  
import java.io.IOException  
import scalaz.zio.clock.Clock  
import eu.timepit.refined.api.Refined  
import com.playground.strategy.Default._  
import scalaz.zio.console.{Console, getStrLn, putStrLn}  
import com.playground.strategy.Common.{Env, isCapitalLetter}   
  
Welcome to Scala version 2.11.5 (Java HotSpot(TM) 64-Bit Server VM, Java 1.7.0_40).  
Type in expressions to have them evaluated.  
Type :help for more information.  
  
scala>
  
In the console, issue the following commands:  
scala> import com.playground.solution.Main._  
scala> main(Array(""))
  
# For ruby script to test the agent

1. go to the directory where the script is in the shell - in data folder.

2. issue the command in the shell ./simulate.rb \<path-to\>/solution.jar \<path-to\>/grids.data.  
   (i.e. ./simulate.rb ../target/scala-2.12/solution.jar grids.data)

You should see:

We start the game at:

y is : 69  
x is : 52
.....

Or you can inspect the sample run session at data/target/debug.txt.
