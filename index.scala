//> using platform scala-js
//> using jsVersion 1.17.0
//> using dep org.scala-js::scalajs-dom::2.8.0
//> using dep tech.neander::cue4s::0.0.3
//> using scala 3.5.2
//> using option -Wunused:all

package cue4s

import org.scalajs.dom.*

import scala.concurrent.ExecutionContext.Implicits.global

import CharCollector.*

def demonstratePrompt[Result](
    prompt: Prompt[Result],
    xterm: XTermTerminal
): Future[Completion[Result]] =
  val xtermOut = XtermOutput(xterm)
  val terminal = AnsiTerminal(xtermOut)
  val handler = prompt.framework(terminal, xtermOut, Theme.Default).handler
  var state = State.Init

  val completion = Promise[Completion[Result]]

  def close(res: Completion[Result]) =
    completion.complete(Success(res))

  def whatNext(n: Next[Result]) =
    n match
      case Next.Continue    =>
      case Next.Done(value) => close(Completion.Finished(value))
      case Next.Stop        => close(Completion.interrupted)
      case Next.Error(msg)  => close(Completion.error(msg))

  def send(ev: cue4s.Event) =
    whatNext(handler(ev))

  xterm.onKey: keypress =>
    keypress.key
      .getBytes()
      .foreach: byte =>
        val (newState, result) = decode(state, byte)

        state = newState

        result match
          case d: CharCollector.DecodeResult =>
            import CharCollector.DecodeResult.*
            d match
              case Continue   => whatNext(Next.Continue)
              case Error(msg) => whatNext(Next.Error(msg))

          case e: cue4s.Event => send(e)

  terminal.cursorHide()

  handler(cue4s.Event.Init)

  completion.future

end demonstratePrompt

def createXterm(element: Element) =
  val xterm = new XTermTerminal
  val fitAddon = new FitAddon
  xterm.loadAddon(fitAddon)
  xterm.open(element)
  fitAddon.fit()

  xterm

def demo[Result](
    prompt: Prompt[Result],
    terminalContainer: Element,
    resultContainer: Element
) =
  val xterm = createXterm(terminalContainer)
  demonstratePrompt(prompt, xterm).onComplete: res =>
    resultContainer.innerHTML = "<b>Result:</b> " + res.toString()
end demo

@main def hello =
  val jq = document.getElementById(_)

  demo(
    new cue4s.Prompt.Input(
      "What color is the sky?"
    ).validate(s => Option.when(s != "blue")(PromptError("it's blue!"))),
    jq("text-input-terminal"),
    jq("text-input-result")
  )

  demo(
    new cue4s.Prompt.PasswordInput(
      "Come up with a password"
    ).validate(s =>
      Option
        .when(s.raw.length < 10)(PromptError("Must be longer than 10 symbols!"))
        .orElse(
          Option
            .when(!s.raw.exists(_.isDigit))(PromptError("Must contain a digit"))
        )
    ),
    jq("password-input-terminal"),
    jq("password-input-result")
  )

  demo(
    new cue4s.Prompt.NumberInput[Int](
      "Choose a number no less than 10"
    ).positive.min(10),
    jq("number-input-terminal"),
    jq("number-input-result")
  )

  demo(
    new cue4s.Prompt.SingleChoice(
      "How are you feeling today",
      List("ok", "acceptable", "fine", "understated")
    ),
    jq("single-choice-input-terminal"),
    jq("single-choice-input-result")
  )

end hello

import scalajs.js, js.annotation.*
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Success

@js.native
trait KeyPress extends js.Any:
  val key: String = js.native

class XtermOutput(term: XTermTerminal) extends Output:
  override def logLn[A: AsString](a: A): Unit = console.log(a)
  override def out[A: AsString](a: A): Unit =
    term.write(a.render)

@JSImport("@xterm/xterm", "Terminal")
@js.native
class XTermTerminal extends js.Any:
  def write(data: String): Unit = js.native
  def open(element: Element): Unit = js.native
  def onKey(a: js.Function1[KeyPress, Unit]): Unit = js.native
  def loadAddon(addon: Addon): Unit = js.native

trait Addon extends js.Any

@JSImport("@xterm/addon-fit")
@js.native
class FitAddon extends js.Any, Addon:
  def fit(): Unit = js.native
