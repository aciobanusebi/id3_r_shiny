// Code taken from https://gist.github.com/withr/8799489

setInterval(function(){
  if ($('html').attr('class')=='shiny-busy' || !$('html')[0].hasAttribute("class")) {
    setTimeout(function() {
      if ($('html').attr('class')=='shiny-busy') {
        $('div.loading').show()
      }
    }, 1000)
  } else {
    $('div.loading').hide()
  }
}, 100);
