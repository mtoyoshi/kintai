package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import models.TimeCard

object Application extends Controller {

  val kintaiForm = Form {
    mapping("content" -> nonEmptyText)(TimeCard.apply)(TimeCard.unapply)
      .verifying(
        "Error has occured! Please check the input format.",
        timeCard => timeCard.validation)
  }

  def index = Action {
    Ok(toIndexPage(kintaiForm))
  }

  def convert = Action { implicit request =>
    kintaiForm.bindFromRequest().fold(
      formWithError => BadRequest(toIndexPage(formWithError)),
      timeCard => Ok(toIndexPage(kintaiForm.fill(timeCard))))
  }

  private def toIndexPage(form: Form[TimeCard]) = views.html.index(form)
}