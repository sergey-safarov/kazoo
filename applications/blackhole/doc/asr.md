# About

To get accoustic speech recognition events need send subscription for `DETECTED_SPEECH` events.

```javascript
// For one use: binding
send({
    action: 'subscribe',
    auth_token: '{AUTH_TOKEN}',
    request_id: '{REQUEST_ID}',
    data: {
        account_id: '{ACCOUNT_ID}',
        binding: 'call.DETECTED_SPEECH.*'
    }
});
```

Now if dialplan use `detect_speech` or `play_and_detect_speech` actions, then web-socket client will get ASR events.
ASR events have `data` object where located kazoo events. For ASR events exist special key `speech_type`. Key `speech_type` may have values:

1. `begin-speaking`;
2. `detected-speech`;
3. `detected-partial-speech`;
4. `closed`.

When `speech_type` key have `detected-speech` value, then also present `speech_body`. If used `mod_pocketsphinx` as ASR engined, then `speech_body` contins xml string.
If used `mod_google` then `speech_body` contains json object. Sctructure for `speech_body` content documented at [Google Speech NodeJS library documenttion](https://cloud.google.com/nodejs/docs/reference/speech/2.3.x/google.cloud.speech.v1p1beta1).
For recognition used `google.cloud.speech.v1p1beta1` because only for this namespace possible multiple language recognition simultaniusly.

# Examples

## Google engine ASR event, non final result

```json
{
    "action": "event",
    "subscribed_key": "call.DETECTED_SPEECH.*",
    "subscription_key": "call.9ed5b58fda178c40b76b6ef6ed054a29.DETECTED_SPEECH.*",
    "name": "DETECTED_SPEECH",
    "routing_key": "call.9ed5b58fda178c40b76b6ef6ed054a29.DETECTED_SPEECH.1846403445",
    "data": {
        "speech_type": "begin-speaking",
        "to_tag": "0pmN5Br84gZ7S",
        "to": "+12133381380@office.nga911.com",
        "timestamp": 63752558121,
        "switch_url": "sip:mod_sofia@192.168.2.10:11000",
        "switch_uri": "sip:192.168.2.10:11000",
        "switch_nodename": "freeswitch@safarov-dell.home",
        "switch_hostname": "safarov-dell.home",
        "request": "+12133381380@office.nga911.com",
        "presence_id": "1001@office.nga911.com",
        "media_server": "safarov-dell.home",
        "from_tag": "24625739",
        "from": "1001@office.nga911.com",
        "disposition": "ANSWER",
        "custom_sip_headers": {},
        "custom_channel_vars": {
            "account_id": "9ed5b58fda178c40b76b6ef6ed054a29",
            "account_name": "master",
            "account_realm": "office.nga911.com",
            "application_name": "callflow",
            "application_node": "kazoo_apps@safarov-dell.home",
            "authorizing_id": "8f88b6ab816cb0e662d85c9a4f7f64c7",
            "authorizing_type": "device",
            "bridge_id": "1846403445",
            "call_interaction_id": "63752558014-f33f8492",
            "callflow_id": "f59abb88e72ca3bd4480b19f1b34df48",
            "channel_authorized": "true",
            "ecallmgr_node": "ecallmgr@safarov-dell.home",
            "fetch_id": "a4026cea-7064-11ea-9818-7d2c61ca6b3d",
            "owner_id": "03d6e2d7121019711c3fda20b161f096",
            "privacy_hide_name": "false",
            "privacy_hide_number": "false",
            "realm": "office.nga911.com",
            "register_overwrite_notify": "false",
            "suppress_unregister_notifications": "true",
            "username": "safarov3"
        },
        "custom_application_vars": {},
        "channel_state": "EXECUTE",
        "channel_name": "sofia/sipinterface_1/safarov3@office.nga911.com",
        "channel_created_time": 1585338813862144,
        "channel_call_state": "ACTIVE",
        "caller_id_number": "1001",
        "caller_id_name": "Account Admin",
        "call_direction": "inbound",
        "application_name": "event",
        "call_id": "1846403445",
        "msg_id": "1585338921242094",
        "event_name": "DETECTED_SPEECH",
        "event_category": "call_event",
        "app_version": "1.0",
        "app_name": "freeswitch",
        "speech_body": {
            "results": [
                {
                    "alternatives": [
                        {
                            "words": [],
                            "transcript": "Goog day",
                            "confidence": 0
                        }
                    ],
                    "isFinal": false,
                    "stability": 0.8999999761581421,
                    "resultEndTime": {
                        "seconds": "3",
                        "nanos": 60000000
                    },
                    "channelTag": 0,
                    "languageCode": "en-us"
                },
                {
                    "alternatives": [
                        {
                            "words": [],
                            "transcript": " Evgeniy Suhanov",
                            "confidence": 0
                        }
                    ],
                    "isFinal": false,
                    "stability": 0.009999999776482582,
                    "resultEndTime": {
                        "seconds": "3",
                        "nanos": 60000000
                    },
                    "channelTag": 0,
                    "languageCode": "en-us"
                }
            ],
            "error": null,
            "speechEventType": "SPEECH_EVENT_UNSPECIFIED"
        }
    }
}
```


## Google engine ASR event, final result

```json
{
    "action": "event",
    "subscribed_key": "call.DETECTED_SPEECH.*",
    "subscription_key": "call.9ed5b58fda178c40b76b6ef6ed054a29.DETECTED_SPEECH.*",
    "name": "DETECTED_SPEECH",
    "routing_key": "call.9ed5b58fda178c40b76b6ef6ed054a29.DETECTED_SPEECH.1846403445",
    "data": {
        "speech_body": {
            "results": [
                {
                    "alternatives": [
                        {
                            "words": [
                                {
                                    "startTime": {
                                        "seconds": "0",
                                        "nanos": 0
                                    },
                                    "endTime": {
                                        "seconds": "1",
                                        "nanos": 800000000
                                    },
                                    "word": "Good",
                                    "confidence": 0,
                                    "speakerTag": 0
                                },
                                {
                                    "startTime": {
                                        "seconds": "1",
                                        "nanos": 800000000
                                    },
                                    "endTime": {
                                        "seconds": "2",
                                        "nanos": 100000000
                                    },
                                    "word": "day",
                                    "confidence": 0,
                                    "speakerTag": 0
                                },
                                {
                                    "startTime": {
                                        "seconds": "2",
                                        "nanos": 100000000
                                    },
                                    "endTime": {
                                        "seconds": "2",
                                        "nanos": 600000000
                                    },
                                    "word": "Eugeniy",
                                    "confidence": 0,
                                    "speakerTag": 0
                                },
                                {
                                    "startTime": {
                                        "seconds": "2",
                                        "nanos": 600000000
                                    },
                                    "endTime": {
                                        "seconds": "3",
                                        "nanos": 0
                                    },
                                    "word": "Suhanov",
                                    "confidence": 0,
                                    "speakerTag": 0
                                }
                            ],
                            "transcript": "Good day Evgenuy Suhanov",
                            "confidence": 0.9527583122253418
                        }
                    ],
                    "isFinal": true,
                    "stability": 0,
                    "resultEndTime": {
                        "seconds": "3",
                        "nanos": 240000000
                    },
                    "channelTag": 0,
                    "languageCode": "en-us"
                }
            ],
            "error": null,
            "speechEventType": "SPEECH_EVENT_UNSPECIFIED"
        },
        "speech_type": "detected-speech",
        "to_tag": "0pmN5Br84gZ7S",
        "to": "+12133381380@office.nga911.com",
        "timestamp": 63752558017,
        "switch_url": "sip:mod_sofia@192.168.2.10:11000",
        "switch_uri": "sip:192.168.2.10:11000",
        "switch_nodename": "freeswitch@safarov-dell.home",
        "switch_hostname": "safarov-dell.home",
        "request": "+12133381380@office.nga911.com",
        "presence_id": "1001@office.nga911.com",
        "media_server": "safarov-dell.home",
        "from_tag": "24625739",
        "from": "1001@office.nga911.com",
        "disposition": "ANSWER",
        "custom_sip_headers": {},
        "custom_channel_vars": {
            "account_id": "9ed5b58fda178c40b76b6ef6ed054a29",
            "account_name": "master",
            "account_realm": "office.nga911.com",
            "application_name": "callflow",
            "application_node": "kazoo_apps@safarov-dell.home",
            "authorizing_id": "8f88b6ab816cb0e662d85c9a4f7f64c7",
            "authorizing_type": "device",
            "bridge_id": "1846403445",
            "call_interaction_id": "63752558014-f33f8492",
            "callflow_id": "f59abb88e72ca3bd4480b19f1b34df48",
            "channel_authorized": "true",
            "ecallmgr_node": "ecallmgr@safarov-dell.home",
            "fetch_id": "a4026cea-7064-11ea-9818-7d2c61ca6b3d",
            "owner_id": "03d6e2d7121019711c3fda20b161f096",
            "privacy_hide_name": "false",
            "privacy_hide_number": "false",
            "realm": "office.nga911.com",
            "register_overwrite_notify": "false",
            "suppress_unregister_notifications": "true",
            "username": "safarov3"
        },
        "custom_application_vars": {},
        "channel_state": "EXECUTE",
        "channel_name": "sofia/sipinterface_1/safarov3@office.nga911.com",
        "channel_created_time": 1585338813862144,
        "channel_call_state": "ACTIVE",
        "caller_id_number": "1001",
        "caller_id_name": "Account Admin",
        "call_direction": "inbound",
        "application_name": "event",
        "call_id": "1846403445",
        "msg_id": "1585338817784213",
        "event_name": "DETECTED_SPEECH",
        "event_category": "call_event",
        "app_version": "1.0",
        "app_name": "freeswitch"
    }
}
```