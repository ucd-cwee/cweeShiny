#' cwee_css
#' 
#' @export

cwee_css <- function () {
  "
  .control-label {font-weight: bold; font-size: 140%;}
         
  hr.style-one {
    border: 0;
    height: 1px;
    background: #333;
    background-image: -webkit-linear-gradient(left, #ccc, #333, #ccc);
    background-image:    -moz-linear-gradient(left, #ccc, #333, #ccc);
    background-image:     -ms-linear-gradient(left, #ccc, #333, #ccc);
    background-image:      -o-linear-gradient(left, #ccc, #333, #ccc);
    clear: both;
  }
         
  hr.solidBlack {
    color: #000;
    background-color: #000;
    height: 2px;
    margin: 3px 0px;
  }
         
  input[id^='downloadPlot_'] {
    width: 50px;
  }
         
  label[for^='downloadPlot_'] {
    display: inline;
  }
         
  #orText {
    float: left;
    margin-right: 10px;
  }

  label[for='entireSystem'] {
    float: left;
    margin-left: 10px;
  }

  "
}
