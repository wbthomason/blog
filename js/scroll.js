$(function() {
    // Stick the #nav to the top of the window
    var nav = $('#nav');
    var navHomeY = nav.offset().top;
    var isFixed = false;
    var $w = $(window);
    $w.scroll(function() {
        var scrollTop = $w.scrollTop();
        var shouldBeFixed = scrollTop > navHomeY;
        if (shouldBeFixed && !isFixed) {
            nav.css({
                width: '100%',
                'justify-content': 'center',
                'align-items': 'center',
                'margin-left': 'auto',
                'margin-right': 'auto',
                position: 'fixed',
                top: 0,
                left: nav.offset().left,
                width: nav.width(),
                'border-bottom': 'thin solid #eee',
            });
            isFixed = true;
        }
        else if (!shouldBeFixed && isFixed)
        {
            nav.css({
                width: '100%',
                'justify-content': 'center',
                'align-items': 'center',
                'margin-left': 'auto',
                'margin-right': 'auto',
                position: 'static',
                'border-bottom': 'none'
            });
            isFixed = false;
        }
    });
});
