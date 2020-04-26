'use strict';

function myScript(){
    return('you can also import other scripts!');
}

$( document ).ready(function() {

    $(btnReverse).on('click', function(){
        $(lblReverseResult).text($(txtReverse).val());
    })
});