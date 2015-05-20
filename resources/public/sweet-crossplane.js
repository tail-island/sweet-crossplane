(function($) {

  $.crossplane = {
    locale: 'en',
    dateFormat: 'MMM D, YYYY',
    timstampFormat: 'MMM D, YYYY HH:MM:SS',

    labels: {
      confirm: 'Confirm',
      yes: 'Yes',
      no: 'No',
    },

    setDefaults: function(options) {
      $.extend(true, $.crossplane, options);
      return this;
    },

    hiddenField: function(name, val) {
      return $('<input type="hidden">').attr('name', name).val(val);
    },

    form: function(uri, method, params) {
      var $form = $('<form>').attr('action', uri);

      if (method === 'get' || method === 'post') {
        $form.attr('method', method);
      } else {
        $form.attr('method', 'post').append($.crossplane.hiddenField('_method', method));
      }

      for (var name in params) {
        $form.append($.crossplane.hiddenField(name, params[name]));
      }

      return $form;
    },

    goToWithMethodAndParams: function(uri, method, params) {
      $.crossplane.form(uri, method, params).submit();
      return this;
    },

    confirmModal: function($element) {
      var $modal = $('<div class="modal fade">' +
                     '  <div class="modal-dialog">' +
                     '    <div class="modal-content">' +
                     '      <div class="modal-header">' +
                     '        <button type="button" class="close" data-dismiss="modal">&times;</span></button>' +
                     '        <h4 class="modal-title"></h4>' +
                     '      </div>' +
                     '      <div class="modal-body"></div>' +
                     '      <div class="modal-footer">' +
                     '        <button class="btn modal-yes"></button>' +
                     '        <button class="btn modal-no" data-dismiss="modal"></button>' +
                     '      </div>' +
                     '    </div>' +
                     '  </div>' +
                     '</div>');

      $modal.find('.modal-title').text($.crossplane.labels.confirm);
      $modal.find('.modal-body' ).text($element.data('confirm'));
      $modal.find('.modal-yes'  ).text($.crossplane.labels.yes);
      $modal.find('.modal-no'   ).text($.crossplane.labels.no);

      $modal.find('.modal-yes').on('click', function() {
        $element.data('confirm', null);

        $element[0].click();  // jQueryのtrigger('click')だと画面遷移しなかったので、DOMのclick()を呼び出しています。
      });

      return $modal;
    },

    getConfirmModal: function($element) {
      var $modal = $element.data('confirm-modal');

      if (!$modal) {
        $modal = $.crossplane.confirmModal($element);
        $element.data('confirm-modal', $modal);
      }

      return $modal;
    },
    
    showConfirmModal: function($element) {
      $.crossplane.getConfirmModal($element).modal('show');
      return this;
    },

    setOpenerSelectProperty: function(id, key, name) {
      if (!window.opener || window.opener.closed) {
        return this;
      }
      
      $(window.opener.document.getElementById(id)).val(key);
      $(window.opener.document.getElementById(id + '-name--')).val(name);
      return this;
    }
  };

  $(function() {
    $('li.disabled a').on('click', function() {
      return false;
    });

    $('a').on('click', function() {
      if (!$(this).data('confirm')) {
        return true;
      }

      $.crossplane.showConfirmModal($(this));
      return false;  // ダイアログの[Yes]ボタンが押されるまで待つために、とりあえず、イベントをキャンセルします。
    });

    $('span[data-open-window-uri].input-group-addon').css('cursor', 'pointer').on('click', function() {
      window.open($(this).data('open-window-uri'), '_blank');
    });
    
    $('div.date').each(function() {
      $(this).datetimepicker({
        locale: $.crossplane.locale,
        format: $(this).data('type') === 'date' ? $.crossplane.dateFormat : $.crossplane.timestampFormat,
        useStrict: true,
        keepInvalid: true
      });
    });
  });

})(jQuery);
