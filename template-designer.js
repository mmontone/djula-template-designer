$(function() {
    $('textarea.ace').each(function() {
        var textarea = $(this);
        console.log('rea', textarea);
        var mode = textarea.data('editor');
        var editDiv = $('<div>', {
            position: 'absolute',
            width: textarea.width(),
            height: textarea.height(),
            'class': textarea.attr('class')
        }).insertBefore(textarea);
        textarea.css('display', 'none');
        var editor = ace.edit(editDiv[0]);
        console.log(editDiv);//break;
        //editor.renderer.setShowGutter(textarea.data('gutter'));
        editor.session.setValue(textarea.val());
        editor.session.setMode("ace/mode/html");
        //editor.setTheme("ace/theme/idle_fingers");
        //editor.resize();

        // copy back to textarea on form submit...
        textarea.closest('form').submit(function() {
            textarea.val(editor.session.getValue());
        })
    });
});
