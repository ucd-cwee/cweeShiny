#' cwee_css
#' 
#' @export

cwee_css <- function () {
  "
  hr.style-one {
    border: 0;
    height: 1px;
    background: #333;
    background-image: -webkit-linear-gradient(left, #ccc, #333, #ccc);
    background-image:    -moz-linear-gradient(left, #ccc, #333, #ccc);
    background-image:     -ms-linear-gradient(left, #ccc, #333, #ccc);
    background-image:      -o-linear-gradient(left, #ccc, #333, #ccc);
  }
  "
}