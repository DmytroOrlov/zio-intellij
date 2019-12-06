package zio.intellij.synthetic.macros

import org.jetbrains.plugins.scala.lang.psi.api.base.literals.ScStringLiteral
import org.jetbrains.plugins.scala.lang.psi.api.base.{ScAnnotation, ScLiteral}
import org.jetbrains.plugins.scala.lang.psi.api.statements.ScFunctionDeclaration
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScObject, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector
import org.jetbrains.plugins.scala.lang.psi.types.PhysicalMethodSignature

class ModulePatternAccessible extends SyntheticMembersInjector {
  private val accessible = Seq(
    "zio.macros.annotation.accessible",
    "zio.macros.accessible",
  )

  private def hasAccessible(source: ScTypeDefinition) =
    accessible.exists(a => source.findAnnotationNoAliases(a) != null)

  private def annotationFirstParam(scAnnotation: ScAnnotation): Option[String] =
    scAnnotation.annotationExpr.getAnnotationParameters.collectFirst {
      case sl: ScStringLiteral => sl.getValue()
    }

  private def helperObjectExtension(name: Option[String], sco: ScObject, plain: Boolean): Seq[String] =
    name.map { n =>
      if (plain) s"def $n : ${sco.qualifiedName} = ???"
      else s"def $n : ${sco.qualifiedName}.Service[${sco.qualifiedName}] = ???"
    }.toSeq

  private def accessorTraitExtension(sco: ScObject): String = {
    val serviceTrait = sco.typeDefinitions.find(_.name == "Service")
    val signatures = serviceTrait.toSeq.flatMap(_.allMethods).collect {
      case PhysicalMethodSignature(method: ScFunctionDeclaration, _) => s"${method.getText} = ???"
    }

    s"""trait Accessors extends ${sco.qualifiedName}.Service[${sco.name}] {" +
           ${signatures.mkString("\n")}
        }"""
  }

  private def accessorMethodsExtension(sco: ScObject): Seq[String] = {
    val serviceTrait = sco.typeDefinitions.find(_.name == "Service")
    val signatures = serviceTrait.toSeq.flatMap(_.allMethods).collect {
      case PhysicalMethodSignature(method: ScFunctionDeclaration, _) => s"${method.getText} = ???"
    }
    signatures
  }

  private def findAccessibleMacroAnnotation(accessible: Seq[String], sco: ScObject): Option[ScAnnotation] = {
    val companion = sco.fakeCompanionClassOrCompanionClass
    accessible.map(companion.getAnnotation).collectFirst {
      case a: ScAnnotation => a
    }
  }

  override def injectMembers(source: ScTypeDefinition): Seq[String] =
    source match {
      case sco: ScObject =>
        val plain = !sco.typeDefinitions.exists(_.name == "Service")
        val annotation17 = findAccessibleMacroAnnotation(accessible.head :: Nil, sco)
        annotation17.fold {
          if (plain) helperObjectExtension(Some(">"), sco, plain)
          else accessorMethodsExtension(sco)
        }(a => helperObjectExtension(annotationFirstParam(a), sco, plain) :+ accessorTraitExtension(sco))
      case _ =>
        Nil
    }

  override def needsCompanionObject(source: ScTypeDefinition) = hasAccessible(source)
}
