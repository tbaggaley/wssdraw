{application,ws_server,
             [{description,"An OTP application"},
              {vsn,"0.1.0"},
              {registered,[]},
              {mod,{ws_server_app,[]}},
              {applications,[cowboy,kernel,stdlib]},
              {env,[]},
              {modules,[broadcaster,ws_handler,ws_server_app,ws_server_sup]},
              {licenses,["Apache 2.0"]},
              {links,[]}]}.