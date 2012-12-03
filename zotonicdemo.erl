%% @author Arjan Scherpenisse
%% @copyright 2012 Arjan Scherpenisse
%% Generated on 2012-11-29
%% @doc This site was based on the 'empty' skeleton.

-module(zotonicdemo).
-author("Arjan Scherpenisse").

-mod_title("Zotonic Demonstration site").
-mod_description("A sandbox environment site, for demonstration purposes.").
-mod_prio(10).
-mod_schema(1).

-include_lib("zotonic.hrl").

-export([manage_schema/2, observe_module_activate/2]).

%%====================================================================
%% API functions go here
%%====================================================================

manage_schema(install, _Context) ->
    #datamodel{resources=[
                          {page_home,
                           text,
                           [{title, <<"Zotonic Demonstration site">>},
                            {summary, <<"This site functions as a demonstration site for Zotonic and the Zotonic admin interface.">>},
                            {body, <<"<p>To edit this page, click the button below to log in to the admin interface.</p>">>}
                           ]}
                         ]}.

observe_module_activate(#module_activate{module=mod_ssl}, Context) ->
    z_module_manager:deactivate(mod_ssl, Context);
observe_module_activate(_, _) ->
    undefined.

%%====================================================================
%% support functions go here
%%====================================================================

