//if(!location.pathname.match("^/ocpu/user/nuno/library/TE/www")){
//opencpu.seturl("http://forecasts.tradingeconomics.com/ocpu/user/nuno/library/TE/R");}  
//opencpu.seturl("../ocpu/library/TE/R")
opencpu.seturl("http://r.ieconomics.com/ocpu/library/TE/R");

var home = "http://r.ieconomics.com";
var baseLoc = "/apps/";
var countries = [];
var indicators = [];
var regions = [];
// var userN = "guest";
// var passW = "guest";
var userN = "r@tradingeconomics.com";
var passW = "Lisboa!1";
var country = [];
var indicator = [];
var startDate = [];
var $tab = $('#myTab');
var firsthand = true;

$('#startDate').datepicker({
//   orientation:'top left',
   format: 'mm-dd-yyyy',
   viewMode: 'months',
   minViewMode: 'months'
});

$("#plotdiv").resizable();

$('#plotdiv').bind('resize', function(){
  $("#sharebutton").attr('disabled', false);
});

function rIndicatorDD(country,callback){
  indicator = $("#indicator").val();
 // console.log(indicator)
  jQuery.getJSON(home+'/indicators/'+country,function(){}).done(function(data) {
  if(data.length<1){return;}
  indicators = []
  for(var i = 0;i<data.length;i++){
    indicators[i]=data[i].cat
  }
  callback()
  })
}
function rCountryDD(indicator,callback){
  country = $("#country").val();
  jQuery.getJSON(home+'/countries/'+indicator,function(){}).done(function(data) {
  if(data.length<1){return;}
  countries = []
  for(var i = 0;i<data.length;i++){
    countries[i]=data[i].c
  }
  callback()
  })
}
function bCountryDD(div,id,callback){
        var cselec="";
         $(div).text('')
        cselec += '<p>'//<b>Country</b>'
        cselec += '<select class="form-control myselec" id="'+id+'">';
        // cselec+= '<option value="Commodity">Commodities</option>'
         for(i=0;i<countries.length;i++){
          cselec+= '<option value="' + countries[i] + '">' + countries[i] + '</option>';}
        cselec+='</select>';
        cselec += '</p>'
        $(div).append(cselec)
        var pre = ['United States'];
        setter(pre,id,callback)
}
function bIndicatorDD(div,id,callback,byDefault){
  //console.log(byDefault)
        var cselec="";
         $(div).text('')
         cselec += '<p>' //<b>Indicator</b>'
         cselec += '<select class="form-control myselec" id="'+id+'">';
           for(i=0;i<indicators.length;i++){
           cselec += '<option value="' + indicators[i] + '">' + indicators[i] + '</option>';}
        cselec += '</select>';
        cselec += '</p>'
        $(div).append(cselec)
          if(byDefault){var pre = byDefault}else{var pre=['Unemployment Rate']};
          setter(pre,id,callback)
}
function bRegionDD(div,id,callback,byDefault){
        var cselec="";
         $(div).text('')
         //cselec = '<b>Region:</b>'
         cselec += '<select class="form-control myselec" id="'+id+'">';
           for(i=0;i<regions.length;i++){
           cselec+= '<option value="' + regions[i] + '">' + regions[i] + '</option>';}
        cselec+='</select>';
        $(div).append(cselec)
          if(byDefault){var pre = byDefault}else{var pre=['Europe']};
          setter(pre,id,callback)
}
function bMultipleCountriesDD(div,id,callback,byDefault){
       var selec = ""
       $(div).text('')
       selec += '<p>'// <b>Countries</b>'
       selec += '<select id="'+id+'" class="form-control myselec" multiple="multiple" size="19">\n'
      for(j=0;j<countries.length;j++)
       {
       selec += '<option value="'+countries[j]+'">'+countries[j]+'</option>'
       }
      selec += '</select>'
      selec += '</p>'
      selec += '<p>'
      selec += '(HINT: Hold <b>CTRL</b> to select several Countries.)<br>'
      $(div).append(selec)
      if(byDefault){var prec = byDefault}else{var prec = ['United States','United Kingdom','China','Euro Area','Germany','Japan','France'];};
      setter(prec,id,callback)     
}
function bMultipleIndicatorsDD(div,id,callback,byDefault){
       var selec = ""
       $(div).text('')
       selec += '<p>'// <b>Indicators</b>'
       selec += '<select id="'+id+'" class="form-control myselec" multiple="multiple" size="19">\n'
       for(j=0;j<indicators.length;j++)
       {
       selec += '<option value="'+indicators[j]+'">'+indicators[j]+'</option>'
       }
      selec += '</select>'
      selec += '</p>'
      selec += '<p>'
      selec += '(HINT: Hold <b>CTRL</b> to select several Indicators.)<br>'
      $(div).append(selec)
      if(byDefault){var prei = byDefault}else{var prei = ['Industrial Production','GDP Annual Growth Rate','Inflation Rate','Unemployment Rate'];};
      setter(prei,id,callback)      
}
function loadInfo(callback){
countries = [];
indicators = [];
  $.getJSON("./indicators.json", function( data1 ) {
    $.each(data1, function(key, val){  
    for(i=0;i<val.length;i++){
    indicators.push(val[i]);}})
    }).always(function() {
      $.getJSON("./countries.json", function( data2 ) {
        $.each(data2, function(key, val){  
        for(i=0;i<val.length;i++){
        countries.push(val[i]);}})
        }).always(function() {
          $.getJSON("./regions.json", function( data3 ) {
            $.each(data3, function(key, val){  
            for(i=0;i<val.length;i++){
            regions.push(val[i]);}})
            }).always(function() {
              callback()          
      });
    });
  });
}     
function rInteractPlot(type, auth, country, indicator, date, opts){
    $("#sharebutton").prop('disabled', true);
    $("#seetable").prop('disabled', true);
    opts = typeof opts !== 'undefined' ? opts : null;
    var myObj = {
          c : auth,
          country : country,
          indicator : indicator,
          d1 : date,
          opts : opts
        }
    
    if( date && new Date()<new Date(date)){
      throwAlert("Invalid time period")
      $('#loadingDiv').hide();
      $("#submitbutton").prop('disabled', false);
      return;
    }   
    if(!(country && indicator) && type!="te.geomap"){
      throwAlert("Please, select parameters")
      $('#loadingDiv').hide();
      $("#submitbutton").prop('disabled', false);
      return;
      // $("#alert").append('<div class="alert alert-warning alert-dismissable">  <button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;</button><strong>Alert!</strong> Please, select parameters.');
    }
    
    if(country.length>70 || indicator.length>70)
    {
      $("#submitbutton").prop('disabled', false);
      $('#loadingDiv').hide();
      throwAlert("Too many indicators to show. Please re-do selection")
      //$("#alert").append('<div class="alert alert-warning alert-dismissable">  <button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;</button> <strong>Alert.</strong> Too many indicators to show. Please re-do selection.</div>');
    }else{
        var req = $("#plotdiv").r_fun_plot(type, myObj)
        req.fail(function(){
          $("#sharebutton").attr('data-ready','true')
          $('#loadingDiv').hide();
          $("#submitbutton").prop('disabled', false);
          logThis(req.responseText)
          throwAlert("We are sorry but at the moment we cannot complete your request. Please, re-check the parameters and let us know if the problem persists","danger")
        //  $("#alert").append('<div class="alert alert-danger alert-dismissable">  <button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;</button> <strong>Process interrupted.</strong>  We are sorry but at the moment we cannot complete your request. Please, re-check the parameters and let us know if the problem persists. </div>');
        });
        req.success(function(){
          $("#sharebutton").attr('data-ready','true')
          $('#loadingDiv').hide();
          $("#submitbutton").prop('disabled', false);
          $("#sharebutton").removeAttr("style")
          $("#sharebutton").prop('disabled', false);
      //  $("#seetable").removeAttr("style")
      //  $("#seetable").prop('disabled', false);
        });
        req.always(function(){
      });      
    }    
  }
function throwAlert(msg,atype){
  $("#alert").text('')
  $("#alert").append('<div class="alert alert-'+ (atype || 'warning') +' alert-dismissable">  <button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;</button><strong>Alert!</strong> '+msg+'.')
}
function rInteractTable(type, auth, country, indicator, date){
  $("#seetable").prop('disabled', true);
     var tbl = opencpu.r_fun_json("te.table", {
          c : auth,
          countries : country,
          indicators : indicator,
          d1 : date,
          what : type
        }, function(output){
         // $('#tablediv').text('!'+JSON.stringify(output));
          $('#tablediv').text('');
          var mt = '<table id="tdiv" class="tablesorter">';
          mt += '<thead>';
          mt += '<tr>';
          mt += '<th>Country</th><th>Category</th><th>DateTime</th><th>Value</th>';
          mt += '</tr>';
          mt += '</thead>';
          mt += '<tbody>';
          for(i=0;i<output.Value.length;i++){
            mt += '<tr><td>'+output.Country[i]+'</td><td>'+output.Category[i]+'</td><td>'+output.DateTime[i]+'</td><td>'+output.Value[i]+'</th></tr>';
          }            
          mt += '</tbody>';
          mt += '</table>';
          mt += '<div id="pager" class="pager" style="top: 1417px; position: absolute;">';
          mt += '<form>';
	      	mt += '<img src="dist/addons/pager/icons/first.png" class="first">';
      		mt += '<img src="dist/addons/pager/icons/prev.png" class="prev">';
	      	mt += '<input type="text" class="pagedisplay">';
      		mt += '<img src="dist/addons/pager/icons/next.png" class="next">';
      		mt += '<img src="dist/addons/pager/icons/last.png" class="last">';
      		mt += '<select class="pagesize">';
    			mt += '<option selected="selected" value="15">15</option>';
	    		mt += '<option value="30">30</option>';
		    	mt += '<option value="50">50</option>';
		    	mt += '<option value="100">100</option>';
      		mt += '</select>';
        	mt += '</form>';
          mt += '</div>';
          $("#tablediv").append(mt);
          
          $("#tdiv").tablesorter({
            theme:'blue',
          }).tablesorterPager({container: $("#pager"),
                              size: 15
          }); 
          
        })

        tbl.fail(function(){
          logThis(tbl.responseText)
          $("#alert").append('<div class="alert alert-warning alert-dismissable">  <button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;</button><strong>ERROR!</strong> We are sorry but at the moment we cannot complete your request. Let us know if the problem persists.');
        }); 
      }      
function shareThis(){ 
  var path = "."
  check()
  //console.log(document.getElementById("the_iframe").attr(""))
  console.log(document.URL)
  $("#embbed").text('')
  $("#sharebutton").attr('disabled', 'disabled');
  url=$('a[target="_blank"]').attr('href')
  url=url.substr(0, url.indexOf('/graph')).substr(url.indexOf("tmp/") + 4)
  console.log(url)
  id=randomString(15)
  if(typeof opts == 'undefined'){opts=null}
  if(typeof startDate == 'undefined'){startDate=null}
  $.ajax({
  url: "/s/",
  data: {
         f:url,
         id:id,
         type:type, 
         country: JSON.stringify(country), 
         indicator: JSON.stringify(indicator), 
         d1: startDate, 
         opts:JSON.stringify(opts),
         c:userN,
         w: $('#plotdiv').width(),
         h: $('#plotdiv').height()
  }
}).done(function(data) {
    var embbed = '<div class="shareit" data-id="'+data+'" data-home="'+ baseLoc +'" data-type="TEanalytics"></div><script type="text/javascript" src="'+home+'/share.js"></script>';
 
  $("#embbed").append('<form action=""><textarea readonly="readonly" rows="2" cols="40" style="overflow: hidden; margin: 2px; width: 476px; height: 48px;">'+embbed+'</textarea></form><button id="preview">Preview!</button>');
  
//  $("#embbed").append("got smth: " + data);
$("#preview").on("click", function(){  
  $("#seetable").prop('disabled', true);
  //$('#tablediv').text('').append(embbed)
  var w = window.open('', '', 'width=1000,height=800,resizeable,scrollbars');
      w.document.write(embbed);
      w.document.close();
});
});
}
function check(){
if (window.top!=window.self) 
  {
  //console.log("This window is not the topmost window! Am I in a frame?")
  baseLoc = getUrlVars()['loc']
  }
else
  { 
  console.log("This window is the topmost window!")
  } 
}
function logThis(message){
  $.ajax({
  url: "http://forecasts.tradingeconomics.com:9999/logs/",
  data: {t:type,
    c : country,
    i : indicator,
    l:message}
});
}
function randomString(length){
    var chars = '000111222333444555666777888999aabbccddeeffgghhiijjkkllmmnnooppqqrrssttuuvvwwxxyyzzABCDEFGHIJKLMNOPQRSTUVWXYZ'
    var result = '';
    for (var i = length; i > 0; --i) result += chars[Math.round(Math.random() * (chars.length - 1))];
    return result;
}
function starts(){
  $('#plotdiv').append('<img src="loader2.gif" id="loadingDiv" height="42" width="42">')
  $('#loadingDiv').show();
  $("#submitbutton").prop('disabled', false);
  $("#sharebutton").on("click", function(){        
    shareThis()
  });
}
function submits(){
  //$('#plotdiv').empty()
  $("#embbed").text('')
  $('#tablediv').text('')
  //$('#plotdiv').append('<img src="loader2.gif" id="loadingDiv" height="42" width="42">')
  $('#loadingDiv').show();
  $("#submitbutton").attr('disabled', 'disabled');
  $('#alert').text('')
}
function getUrlVars(){
    var vars = [], hash;
    var hashes = window.location.href.slice(window.location.href.indexOf('?') + 1).split('&');
    for(var i = 0; i < hashes.length; i++)
    {
        hash = hashes[i].split('=');
        vars.push(hash[0]);
        vars[hash[0]] = hash[1];
    }
    return vars;
}
function dcode(json){
  na=(JSON.stringify(json).replace(/\]/g,' ').replace(/\[/g,' ').replace(/\+/g,' ').replace(/\\/g,' ').replace(/\"/g,' ')).split(',');
  for(var i = 0; i < na.length; i++)
    na[i] = na[i].trim();
  return na
}
function doset(selec,type){
$('#'+type+' option').attr("selected", false);
    for(i=0;i<selec.length;i++){
    $('#'+type+' option[value="'+selec[i]+'"]').attr('selected','selected');
  }
}
function setter(selec,type,callback){
  if(typeof getUrlVars()['id'] != 'undefined'){
//console.log('with ID ')
  var req = $.ajax({
    url: "http://forecasts.tradingeconomics.com:9999/inf/"+getUrlVars()['id']
  });
  req.always(function(){
    info = jQuery.parseJSON(req.responseText);
    $("#plotdiv").css("width",info.w);
    $("#plotdiv").css("height",info.h);
//$("#tablediv").append(info.type);
    switch(info.type)
    {
      case 'te.plot':
      case 'te.plot.compare.scale':
      case 'te.heat.map':
      case 'te.tree.map':
      case 'te.pie.chart':
       switch(type){
         case 'country':
//$("#tablediv").append("+"+dcode(info.c)[1]+"+");
           doset(dcode(info.c),type);
           break;
        case 'indicator':
           doset(dcode(info.i),type);
           break;
       }
       break;
      case 'te.simplecorrelation.matrix':
      case 'te.correlation.matrix':
      case 'te.plot.multi':
      $active = $tab.find('.active');
      var shouldbe=1;
      if(dcode(info.c).length<2&&dcode(info.i).length>1){shouldbe=2}
      if(dcode(info.c).length>1&&dcode(info.i).length>1){shouldbe=3}
//$("#tablediv").text(shouldbe+' + '+$active.attr("data-num"))
      if($active.attr("data-num")==shouldbe || !firsthand){
      if($active.attr("data-num")!=shouldbe){callback = function(){}}
       switch(type){
         case 'country':
//$("#tablediv").append("+"+dcode(info.c)[1]+"+");
           doset(dcode(info.c),type);
           break;
        case 'indicator':
           doset(dcode(info.i),type);
           break;
       }
      }else{
      //if($active.attr("data-num")!=shouldbe){
        $('a[data-num="'+shouldbe+'"]').tab('show');
        firsthand=false
      }
       break;
      case 'te.plot.compare':
       switch(type){
         case 'country':
           doset(dcode(info.c),type);
           break;
        case 'indicator1':
           doset([dcode(info.i)[0]],type);
           break;
        case 'indicator2':
           doset([dcode(info.i)[1]],type);
           break;
       }
       break;
      case 'te.geomap':
           doset(dcode(info.opts),'region');
       switch(type){
        case 'indicator':
           doset(dcode(info.i),type);
           break;
       }
       break;
      default:
      console.log('not found')
  }
  //callback()
});  

req.then(function()
{
callback()
})

  }else{
//  req = req.replace(/\+/g,' ')
//  reqarray=req.split(',');
    doset(selec,type)
    callback()
}
  
}
function test(){
  $("#tablediv").text($("#sharebutton").attr('data-ready')+'?')
}