#!/bin/sh
exec scala -savecompiled "$0" $@
!#

/*
 * Copyright (c) 2016 Evgenii Dobrovidov
 * This file is part of "Web server PoC in Scala".
 *
 * "Web server PoC in Scala" is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * "Web server PoC in Scala" is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with "Web server PoC in Scala".  If not, see <http://www.gnu.org/licenses/>.
 */

import java.io.{File, FileInputStream}
import java.time.LocalDateTime
import java.time.format.{DateTimeFormatter, FormatStyle}

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import java.net.InetSocketAddress

import java.util.concurrent.{SynchronousQueue, ThreadPoolExecutor, TimeUnit}

object Assignment extends App {
  final val listenPort = 8765;

  val server = HttpServer.create(new InetSocketAddress(listenPort), 0)

  server.setExecutor(new ThreadPoolExecutor(0 /*no thread will hang waiting*/, 10 /*max 10 threads*/,
                                            5L /*thread is killed after 5 seconds of idle time*/, TimeUnit.SECONDS,
                                            new SynchronousQueue[Runnable]))

  server.createContext("/banner/", new AssetsHandler)
  server.createContext("/stats", new StatisticsHandler)

  server.start()

  println(s"HTTP server listening on port ${listenPort}")
  println("Press ENTER to terminate...")

  System.in.read()
  server.stop(0)
}

/**
  * Banner display statistics storage class.
  */
object BannerStatistics {

  /**
    * An associative array for storing statistics of banner impressions. Declared @volatile because its value can be used
    * from different threads.
    */
  @volatile private var statistics = scala.collection.mutable.Map(
    1 -> 0,
    2 -> 0,
    3 -> 0
  )

  /**
    * Increases the hit counter of the specified banner by one.
    *
    * @param bannerId ID of the banner for which the counter should be incremented.
    */
  def increment(bannerId: Int) = {
    if (statistics.contains(bannerId)) statistics(bannerId) = statistics(bannerId) + 1
  }

  /**
    * Returns an associative array with statistics of banner impressions, where keys are banner IDs and values are
    * number of impressions.
    *
    * @return Banner display statistics.
    */
  def getStatistics = {
    statistics.clone
  }

}

/**
  * Storage class for paths to banner files. Designed for greater system flexibility (so that file names are not tied
  * to any properties of these banners).
  */
object BannerFilenames {

  /**
    * Associative array for storing paths to banner files.
    */
  private final val filenames = Map(
    1 -> "./assets/b1.gif",
    2 -> "./assets/b2_flexibility_is_best.gif",
    3 -> "./assets/b3.gif"
  )

  /**
    * Returns the specified path to the banner with the specified ID.
    *
    * @param bannerId Banner ID.
    *
    * @return Path to the banner file.
    */
  def getFilename(bannerId: Int) = {
    if (filenames.contains(bannerId)) {
      filenames(bannerId)
    } else {
      null
    }
  }

}

/**
  * Class for processing requests to the page with statistics of banner impressions.
  * Generates a page with statistics data based on a template (./views/statistics.html).
  */
class StatisticsHandler extends HttpHandler {

  private lazy val cachedTemplateContent = {
    //get the content of our impromptu template using Scala tools
    val fileHandle = scala.io.Source.fromFile("./views/statistics.html")(scala.io.Codec.UTF8)
    try fileHandle.mkString finally fileHandle.close()
  };

  /**
    * The main method for processing the incoming request.
    *
    * @param exchanger An object that encapsulates the properties and methods of both the request and the response.
    */
  override def handle(exchanger: HttpExchange) = {

    val requestedPath = exchanger.getRequestURI.getPath
    if (requestedPath.nonEmpty) {
      println(LocalDateTime.now.format(DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM))
        + ": request: "
        + requestedPath
        + " [" + requestedPath.split('/').last + "]");

      try {
        var viewContent = cachedTemplateContent

        //get the output string from the template
        val templateRegexp = "<tr[^>]+data-template[^>]*>.*?</tr>".r
        val templateRow = templateRegexp.findFirstIn(viewContent).get

        //create the final page, but first sort the statistics data by banner IDs,
        //because order of elements in scala.collection.mutable.Map is not implementation defined
        val sortedMap = scala.collection.immutable.ListMap(BannerStatistics.getStatistics.toSeq.sortBy(_._1) : _*)

        var resultContent = ""
        for ((id, count) <- sortedMap) {
          var outputRow = templateRow

          outputRow = outputRow.replaceFirst("#", id.toString)
          outputRow = outputRow.replaceFirst("#", BannerFilenames.getFilename(id))
          outputRow = outputRow.replaceFirst("#", count.toString)

          resultContent += outputRow + "\n"
        }

        //and replace the template string with the final content
        viewContent = viewContent.replace(templateRow, resultContent)

        //and finally give the page to the user
        val outputStream = exchanger.getResponseBody
        val body = viewContent.getBytes("UTF8")

        val headers = exchanger.getResponseHeaders
        headers.set("Content-Type", "text/html; charset=utf-8")
        exchanger.sendResponseHeaders(200, body.length)

        outputStream.write(body)
        outputStream.close()

      } catch {
        case any: Throwable =>
          println(LocalDateTime.now.format(DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM))
            + ": error rendering output: "
            + s"[${any.getClass.toString}] " + any.getMessage);
      }
    }
  }

}

/**
  * Class for processing requests to static resources (as part of a task - to the /banner directory)
  * Opens a stream and sends it to output, pre-sets the response content type in headers.
  */
class AssetsHandler extends HttpHandler {

  /**
    * The main method for processing the incoming request.
    *
    * @param exchanger An object that encapsulates the properties and methods of both the request and the response.
    */
  override def handle(exchanger: HttpExchange) {

    val requestedPath = exchanger.getRequestURI.getPath
    if (requestedPath.nonEmpty) {
      println(LocalDateTime.now.format(DateTimeFormatter.ofLocalizedDateTime(FormatStyle.MEDIUM))
              + ": request: "
              + requestedPath
              + " [" + requestedPath.split('/').last + "]");

      //respond with the file if it exists
      //define the path in the simplest way - take the last part of the URL, separated by slashes
      //we don't have to worry about trying to break anything by asking for return paths (/../) because before the filename anyway
      //character 'b' is substituted
      try {
        val bannerId = requestedPath.split('/').last.toInt
        val bannerFilename = BannerFilenames.getFilename(bannerId)

        if (bannerFilename != null) {
          val requestedFile = new File(bannerFilename)
          if (requestedFile.exists) {

            //we will not process possible errors, because considering architecture com.sun.net.httpserver.*
            //this would require creating an intermediate buffer for reading the file to match the response headers
            //with the result of reading from the file system. But during the transfer of the temporary buffer to the response body, an error may also occur,
            //and the headers are already set by this point, so it still doesn't give a normal error to the user

            val headers = exchanger.getResponseHeaders
            headers.set("Content-Type", "image/gif")

            exchanger.sendResponseHeaders(200, 0)

            val outputStream = exchanger.getResponseBody
            val fileInputStream = new FileInputStream(requestedFile)

            //send a read stream from a file to an output stream in response to a request using Scala tools
            Iterator
              .continually(fileInputStream.read) //set the function to be called on each iteration
              .takeWhile(-1 != _) //set the predicate for the loop
              .foreach(outputStream.write) //set the output function and run the loop

            outputStream.close()

            //calculate impression statistics
            BannerStatistics.increment(bannerId)

          } else {
            respondNotFound(exchanger)
          }

        } else {
          respondNotFound(exchanger)
        }

      } catch {
        case any: Throwable => respondBadRequest(exchanger)
      }

    } else {
      respondNotFound(exchanger)
    }
  }

  /**
    * Sets the 404 code (Not Found) in the header and body of the response to the specified request.
    *
    * @param exchanger An object that encapsulates the properties and methods of both the request and the response.
    */
  private def respondNotFound(exchanger: HttpExchange) = {
    val outputStream = exchanger.getResponseBody

    val body = "HTTP 404 Not found".getBytes

    exchanger.sendResponseHeaders(404, body.length)
    outputStream.write(body)
    outputStream.close()
  }

  /**
    * Sets the 400th code (Bad Request) in the header and body of the response to the specified request.
    *
    * @param exchanger An object that encapsulates the properties and methods of both the request and the response.
    */
  private def respondBadRequest(exchanger: HttpExchange) = {
    val outputStream = exchanger.getResponseBody

    val body = "HTTP 400 Bad Request".getBytes

    exchanger.sendResponseHeaders(404, body.length)
    outputStream.write(body)
    outputStream.close()
  }

}


