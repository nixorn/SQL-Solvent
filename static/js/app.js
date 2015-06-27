function getCanvas(){
    var toReturn = null;
    $.ajax({
        url: 'canvas',
        type: 'get',
        dataType: 'html',
        async: false,
        success: function(data) {
            toReturn = data;
        }
    });
    return toReturn;
}

function deserializeHaskell(serializedData){
    return jsonlite.parse(serializedData.replace(/ /g,"&nbsp;"));
}
function extractTables(array){
    var toReturn = "";
    array[0].forEach(function(item) {
        toReturn += "<div class='table' id='table" + item[0] + "'>" + item[0] + " " + item[1] + " " + item[2] + "</div>";
    });
    return toReturn;
}
function extractLinks(array){
    var toReturn = "";
    array[1].forEach(function(item) {
        toReturn += "<div class='link' id='link" + item[2][0] + "'>" + item[2][0] + " " + item[0] + ":" + item[2][1][0] + "->" + item[1] + ":" + item[2][1][1] + "</div>";
    });
    return toReturn;
}
function updateHighlights(array){
    array[2][0][0].forEach(function(item){
        if (item[1] == 'True') {
            $('#table'+item[0]).addClass('highlighted');
        } else {
            $('#table'+item[0]).removeClass('highlighted');
        }
    });
    array[2][0][1].forEach(function(item){
        if (item[1] == 'True') {
            $('#link'+item[0]).addClass('highlighted');
        } else {
            $('#link'+item[0]).removeClass('highlighted');
        }
    });
}
// action = {on|off}, what = {tables,relations}, items = 241,843
function manageHighlights(action,what,items){
    if (action == 'on') { var url = 'hilight'; } else { var url = 'unlight'; }
    if (what == 'tables') { var data = "[[" + items + "],[]]"; }
    else { var data = "[[],[" + items + "]]"; }
    $.ajax({
            url: url,
            dataType: 'text',
            cache: false,
            contentType: false,
            processData: false,
            data: data,
            type: 'post',
            success: function(){
                var canvas = deserializeHaskell(getCanvas());
                updateHighlights(canvas);
            }
    });
}
$( document ).ready(function() {
    $('#upload').on('click', function(){
        if (typeof $('#fileToUpload').prop('files')[0] !== 'undefined') {
            var file_data = $('#fileToUpload').prop('files')[0];
            var form_data = new FormData();
            form_data.append('file', file_data);
            $.ajax({
                    url: 'upload',
                    dataType: 'text',
                    cache: false,
                    contentType: false,
                    processData: false,
                    data: form_data,
                    type: 'post',
                    success: function(){
                        $('.status').text('DONE!');
                    }
            });
            $('.status').text('Parsing file... Please wait.');
        } else {
            $('.status').text('No file selected.');
        }
    });
    $('#ask').on('click', function(){
        var tables = '["'+$('#requested-table').val()+'"]';
        $.ajax({
                url: 'addtables',
                dataType: 'text',
                cache: false,
                contentType: false,
                processData: false,
                data: tables,
                type: 'post',
                success: function(){
                    updateCanvas();
                }
        });
    });
    $('#update').on('click', function(){
        $(".output").text(getCanvas());
    });
    $('#deserialize').on('click', function(){
        var canvas = deserializeHaskell(getCanvas());
        $(".output").html( extractTables(canvas) +  "-------<br>" + extractLinks(canvas) );
        updateHighlights(canvas);
    });
    $('#hitable').on('click', function(){
        manageHighlights('on', 'tables', $('#table-name').val());
    });
    $('#unhitable').on('click', function(){
        manageHighlights('off', 'tables', $('#table-name').val());
    });
    $('#hilink').on('click', function(){
        manageHighlights('on', 'relations', $('#link-name').val());
    });
    $('#unhilink').on('click', function(){
        manageHighlights('off', 'relations', $('#link-name').val());
    });
    $('#visualize').on('click', function(){
        var canvas = deserializeHaskell(getCanvas());

        var newdata = {};
        var nodes = [];
        var links = [];
        canvas[1].forEach(function(item,i){
            links[i] = {};
            links[i]['id'] = item[2][0];
            links[i]['source'] = {};
            links[i]['source']['name'] = item[2][1][0];
            links[i]['source']['group'] = item[0];
            nodes.push(links[i]['source']);
            links[i]['target'] = {};
            links[i]['target']['name'] = item[2][1][1];
            links[i]['target']['group'] = item[1];
            nodes.push(links[i]['target']);
            links[i]['value'] = 1;
        });
        newdata['nodes'] = nodes;
        newdata['links'] = links;
        data = newdata;
        data.helpers = {left: {}, right: {}};
        init();
    })
});
