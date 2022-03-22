demoMovingPlotsPane <- function()
{
  hist(rnorm(100))
  hist(rnorm(1000))
  Sys.sleep(2)
  KeyboardSimulator::keybd.press("Ctrl+6")
  Sys.sleep(2)
  KeyboardSimulator::keybd.press("Ctrl+9")
  Sys.sleep(2)
}  