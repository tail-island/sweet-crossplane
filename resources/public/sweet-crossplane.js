(function($) {

  $.crossplane = {
    datetimepickerSelector: 'div.date',
    confirmSelector: 'a[data-confirm]',
    
    locale: 'en',
    dateFormat: 'MMM D, YYYY',
    timstampFormat: 'MMM D, YYYY HH:MM:SS',
    
    labels: {
      confirm: 'Confirm',
      ok: 'OK',
      cancel: 'Cancel',
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

    confirm: function(message) {
      return window.confirm(message);
    }
  };

  $(function() {
    $($.crossplane.datetimepickerSelector).each(function() {
      $(this).datetimepicker({
        locale: $.crossplane.locale,
        format: $(this).data('type') === 'date' ? $.crossplane.dateFormat : $.crossplane.timestampFormat
      });
    });
    
    $($.crossplane.confirmSelector).on('click', function(event) {
      if (!$.crossplane.confirm('Are you sure?')) {
        event.preventDefault();
      }
    });
  });

})(jQuery);
