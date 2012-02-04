{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module HtmlCat.Html (html) where
import Text.Hamlet (Html, shamlet)

html :: Html
html = [shamlet|
!!!
<html>
  <head>
    <title>htmlcat
    <script type="text/javascript">
      window.onload = function () {
        var es = new EventSource("/stream");
        es.onmessage = function(event) {
          var data = {};
          data.html = event.data;
          if (!data.html) {
            return;
          }
      
          if (window.scrollY + document.documentElement.clientHeight >= document.documentElement.scrollHeight) {
            var scrollToBottom = true;
          }
  
          var div = document.createElement('div');
          div.innerHTML = data.html + "\n";
  
          var out = document.getElementById('out');
          while (div.firstChild) {
            out.appendChild(div.firstChild);
          }
  
          document.title = data.html.replace(/<.*?>/g, '') + ' - htmlcat';
  
          if (scrollToBottom) {
            window.scrollTo(0, document.body.scrollHeight);
          }
        };
      };
  <body>
    <pre id="out">
|]
