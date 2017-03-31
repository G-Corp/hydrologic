-define(WARNING(Warnings, Options),
        hydrologic_utils:log(warning, ?MODULE, Warnings, Options)).
-define(ERROR(Errors, Options),
        hydrologic_utils:log(error, ?MODULE, Errors, Options)).
