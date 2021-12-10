:- use_module(library(pce)).

:- pce_begin_class(demo, dialog).
/* Instance variables*
         name,   type,  access, description */
variable(result, name*, get,    "Result from Prolog Callback").

initialise(Demo) :->
    "Create something that get/4 and send/3 can work with."::
    send(Demo, send_super, initialise, 'Demo'),
    send(Demo, append, new(InputText, text_item(input, 'Demo Text'))),
    send(Demo, append,
         button(execute_command,
            and(message(@prolog,doIt, Demo,InputText?selection),
            message(@prolog,printIt,Demo)))).
:- pce_end_class.