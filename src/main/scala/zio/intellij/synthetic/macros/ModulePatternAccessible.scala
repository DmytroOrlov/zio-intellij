package zio.intellij.synthetic.macros

import org.jetbrains.plugins.scala.lang.psi.api.base.ScAnnotation
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDeclaration
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector
import org.jetbrains.plugins.scala.lang.psi.types.PhysicalMethodSignature

class ModulePatternAccessible extends SyntheticMembersInjector {
  private val accessible = "zio.macros.accessible"

  private def hasAccessible(source: ScTypeDefinition) =
    source.findAnnotationNoAliases(accessible) != null

  private def accessorMethodsExtension(sco: ScObject): Seq[String] = {
    val serviceTrait = sco.typeDefinitions.find(_.name == "Service")
    val signatures = serviceTrait.toSeq.flatMap(_.allMethods).collect {
      case PhysicalMethodSignature(method: ScFunctionDeclaration, _) => s"${method.getText} = ???"
    }
    signatures
  }

  private def findAccessibleMacroAnnotation(sco: ScObject): Option[ScAnnotation] = {
    val companion = sco.fakeCompanionClassOrCompanionClass
    Option(companion.getAnnotation(accessible)).collect {
      case a: ScAnnotation => a
    }
  }

  override def injectMembers(source: ScTypeDefinition): Seq[String] =
    source match {
      case sco: ScObject =>
        val annotation = findAccessibleMacroAnnotation(sco)
        annotation.map(a => accessorMethodsExtension(sco)).getOrElse(Nil)
      case _ =>
        Nil
    }

  override def needsCompanionObject(source: ScTypeDefinition) = hasAccessible(source)
}
