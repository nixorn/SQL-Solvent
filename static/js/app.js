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
                    success: function(response){
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
                success: function(response){
                    $.get( "canvas", function( data ) {
                        $( ".output" ).text( data );
                        alert( "Load was performed." );
                    });
                }
        });
    });
});
