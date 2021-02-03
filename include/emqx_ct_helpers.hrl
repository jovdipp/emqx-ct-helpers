%%--------------------------------------------------------------------
%% Copyright (c) 2021 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-define( CERTS_PATH(CertName), filename:join( [ "etc", "certs", CertName ]) ).

-define( MQTT_SSL_TWOWAY, [ { cacertfile, ?CERTS_PATH( "cacert.pem" ) },
                            { verify, verify_peer },
                            { fail_if_no_peer_cert, true } ] ).

-define( MQTT_SSL_CLIENT_CERTS, [ { keyfile,    ?CERTS_PATH( "client-key.pem" ) },
                                  { cacertfile, ?CERTS_PATH( "cacert.pem" ) },
                                  { certfile,   ?CERTS_PATH( "client-cert.pem" ) } ] ).

-define( TLS_1_3_CIPHERS, [ { versions, [ 'tlsv1.3' ] },
                            { ciphers,  [ "TLS_AES_256_GCM_SHA384",
                                          "TLS_AES_128_GCM_SHA256",
                                          "TLS_CHACHA20_POLY1305_SHA256",
                                          "TLS_AES_128_CCM_SHA256",
                                          "TLS_AES_128_CCM_8_SHA256"
                                        ] }
                          ]).

-define( TLS_OLD_CIPHERS,  [ { versions, [ 'tlsv1.1', 'tlsv1.2' ] },
                             { ciphers,  [ "ECDHE-ECDSA-AES256-GCM-SHA384",
                                           "ECDHE-RSA-AES256-GCM-SHA384",
                                           "ECDHE-ECDSA-AES256-SHA384",
                                           "ECDHE-RSA-AES256-SHA384",
                                           "ECDHE-ECDSA-DES-CBC3-SHA",
                                           "ECDH-ECDSA-AES256-GCM-SHA384",
                                           "ECDH-RSA-AES256-GCM-SHA384",
                                           "ECDH-ECDSA-AES256-SHA384",
                                           "ECDH-RSA-AES256-SHA384",
                                           "DHE-DSS-AES256-GCM-SHA384",
                                           "DHE-DSS-AES256-SHA256",
                                           "AES256-GCM-SHA384",
                                           "AES256-SHA256",
                                           "ECDHE-ECDSA-AES128-GCM-SHA256",
                                           "ECDHE-RSA-AES128-GCM-SHA256",
                                           "ECDHE-ECDSA-AES128-SHA256",
                                           "ECDHE-RSA-AES128-SHA256",
                                           "ECDH-ECDSA-AES128-GCM-SHA256",
                                           "ECDH-RSA-AES128-GCM-SHA256",
                                           "ECDH-ECDSA-AES128-SHA256",
                                           "ECDH-RSA-AES128-SHA256",
                                           "DHE-DSS-AES128-GCM-SHA256",
                                           "DHE-DSS-AES128-SHA256",
                                           "AES128-GCM-SHA256",
                                           "AES128-SHA256",
                                           "ECDHE-ECDSA-AES256-SHA",
                                           "ECDHE-RSA-AES256-SHA",
                                           "DHE-DSS-AES256-SHA",
                                           "ECDH-ECDSA-AES256-SHA",
                                           "ECDH-RSA-AES256-SHA",
                                           "AES256-SHA",
                                           "ECDHE-ECDSA-AES128-SHA",
                                           "ECDHE-RSA-AES128-SHA",
                                           "DHE-DSS-AES128-SHA",
                                           "ECDH-ECDSA-AES128-SHA",
                                           "ECDH-RSA-AES128-SHA",
                                           "AES128-SHA"
                                         ] }
                            ]).
