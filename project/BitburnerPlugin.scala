import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.fastLinkJSOutput
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.fullLinkJSOutput
import java.nio.file.Files
import java.nio.file.StandardCopyOption.REPLACE_EXISTING

object BitburnerPlugin extends AutoPlugin {
  override def requires: Plugins = ScalaJSPlugin

  object autoImport {
    val uploadToBitburner = taskKey[Unit]("upload js file to bitburner")
  }

  import autoImport._

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    uploadToBitburner := {
      val projectName = name.value
      val sourcePath = (Compile / fullLinkJSOutput).value / "main.js"
      val uploadPath =
        file(".").getCanonicalFile() / "dist" / s"${projectName}.js"
      val parentDir = uploadPath.getParentFile()

      println(s"copy file from:${sourcePath} to ${uploadPath}")

      if (!parentDir.exists()) {
        parentDir.mkdirs()
      }

      Files.copy(sourcePath.toPath, uploadPath.toPath, REPLACE_EXISTING)
    }
  )
}
