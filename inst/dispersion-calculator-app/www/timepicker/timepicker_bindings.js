(function($) {
    var datetimeInputBinding = new Shiny.InputBinding();
    $.extend(datetimeInputBinding, {
        find: function(scope)  {
            return $(scope).find('.shiny-datetime-input');
        },
        getId: function(el) {
            return $(el).attr('id');
        },
        getValue: function(el) {
            //return $(el).datetimepicker().val();
            var $input = $(el).find('input');
            var value = $input.val()
            return value;
        },
        setValue: function(el, value) {
            var date = new Date(value);
            var offset = date.getTimezoneOffset() * 60 * 1000;
            date = new Date(date.getTime() + offset)
            var $input = $(el).find('input');
            var datetimepicker = $input.data('xdsoft_datetimepicker');
            datetimepicker.setOptions({
                value: date
            })
        },
        _parseDate: function(date_str) {
            var date = new Date(date_str);
            var offset = date.getTimezoneOffset() * 60 * 1000;
            return new Date(date.getTime() + offset)
        },
        initialize: function(el) {
            //var date = new Date($(el).data('initial-value'));
            var $input = $(el).find('input');

            var value = $input.data('initial-value');
            if (value !== undefined) {
                $input.datetimepicker({
                    value: value,
                    format: 'm/d/Y H:i',
                    opened: true
                });
            } else {
                $input.datetimepicker({
                    format: 'm/d/Y H:i'
                });
            }

            var dtimepicker = $input.data('xdsoft_datetimepicker');

            var max = $input.data('max-date')
            if (max !== undefined) {
                max = this._parseDate(max);
                dtimepicker.setOptions({
                    maxDate: max
                })
            }

            var min = $input.data('min-date');
            if (min !== undefined) {
                min = this._parseDate(min);
                dtimepicker.setOptions({
                    minDate: min
                })
            }
        },
        receiveMessage: function(el, data) {
            var $input = $(el).find('input');
            var dtimepicker = $input.data('xdsoft_datetimepicker');
            var opts = {};

            if (data.hasOwnProperty('value') && data.value != null) {
                opts.value = data.value
            }

            if (data.hasOwnProperty('max') && data.max != null) {
                var max = this._parseDate(data.max);
                opts.maxDate = max
            }

            if (data.hasOwnProperty('min') && data.min != null) {
                var min = this._parseDate(data.min);
                opts.minDate = min
            }

            dtimepicker.setOptions(opts);

            $(el).trigger('change');
        },
        subscribe: function(el, callback) {
            var elem = $(el)
            elem.on('change.shiny-datetime-input', function(e) {
                callback();
            });
        },
        unsubscribe: function(el) {
            $(el).off('.shiny-datetime-input')
        }
    });

    Shiny.inputBindings.register(datetimeInputBinding);
})(jQuery);
