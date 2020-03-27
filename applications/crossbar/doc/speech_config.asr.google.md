# Speech Config for Google provider

## About Speech Config for Google provider

Speech Sonfig for Google provider used for accoustic speech recognition via Google Speech-to-Text service.

#### Schema
Schema for google speech config set



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`asr_config` | ASR config for one of providers | [#/definitions/speech_config.asr.google](#speech_configasr.google) | [#/definitions/speech_config.asr.pocketsphinx](#speech_configasr.pocketsphinx) |   | `false` |  
`name` | Name for the provider | `string(1..)` |   | `true` |  



Schema for google asr_config set



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`alternativeLanguageCodes.[]` |   | `string('af-ZA' | 'am-ET' | 'hy-AM' | 'az-AZ' | 'id-ID' | 'ms-MY' | 'bn-BD' | 'bn-IN' | 'ca-ES' | 'cs-CZ' | 'da-DK' | 'de-DE' | 'en-AU' | 'en-CA' | 'en-GH' | 'en-GB' | 'en-IN' | 'en-IE' | 'en-KE' | 'en-NZ' | 'en-NG' | 'en-PH' | 'en-SG' | 'en-ZA' | 'en-TZ' | 'en-US' | 'es-AR' | 'es-BO' | 'es-CL' | 'es-CO' | 'es-CR' | 'es-EC' | 'es-SV' | 'es-ES' | 'es-US' | 'es-GT' | 'es-HN' | 'es-MX' | 'es-NI' | 'es-PA' | 'es-PY' | 'es-PE' | 'es-PR' | 'es-DO' | 'es-UY' | 'es-VE' | 'eu-ES' | 'fil-PH' | 'fr-CA' | 'fr-FR' | 'gl-ES' | 'ka-GE' | 'gu-IN' | 'hr-HR' | 'zu-ZA' | 'is-IS' | 'it-IT' | 'jv-ID' | 'kn-IN' | 'km-KH' | 'lo-LA' | 'lv-LV' | 'lt-LT' | 'hu-HU' | 'ml-IN' | 'mr-IN' | 'nl-NL' | 'ne-NP' | 'nb-NO' | 'pl-PL' | 'pt-BR' | 'pt-PT' | 'ro-RO' | 'si-LK' | 'sk-SK' | 'sl-SI' | 'su-ID' | 'sw-TZ' | 'sw-KE' | 'fi-FI' | 'sv-SE' | 'ta-IN' | 'ta-SG' | 'ta-LK' | 'ta-MY' | 'te-IN' | 'vi-VN' | 'tr-TR' | 'ur-PK' | 'ur-IN' | 'el-GR' | 'bg-BG' | 'ru-RU' | 'sr-RS' | 'uk-UA' | 'he-IL' | 'ar-IL' | 'ar-JO' | 'ar-AE' | 'ar-BH' | 'ar-DZ' | 'ar-SA' | 'ar-IQ' | 'ar-KW' | 'ar-MA' | 'ar-TN' | 'ar-OM' | 'ar-PS' | 'ar-QA' | 'ar-LB' | 'ar-EG' | 'fa-IR' | 'hi-IN' | 'th-TH' | 'ko-KR' | 'zh-TW' | 'yue-Hant-HK' | 'ja-JP' | 'zh-HK' | 'zh')` |   | `false` |  
`alternativeLanguageCodes` | A list of up to 3 additional BCP-47 language tags, listing possible alternative languages of the supplied audio | `array(string('af-ZA' | 'am-ET' | 'hy-AM' | 'az-AZ' | 'id-ID' | 'ms-MY' | 'bn-BD' | 'bn-IN' | 'ca-ES' | 'cs-CZ' | 'da-DK' | 'de-DE' | 'en-AU' | 'en-CA' | 'en-GH' | 'en-GB' | 'en-IN' | 'en-IE' | 'en-KE' | 'en-NZ' | 'en-NG' | 'en-PH' | 'en-SG' | 'en-ZA' | 'en-TZ' | 'en-US' | 'es-AR' | 'es-BO' | 'es-CL' | 'es-CO' | 'es-CR' | 'es-EC' | 'es-SV' | 'es-ES' | 'es-US' | 'es-GT' | 'es-HN' | 'es-MX' | 'es-NI' | 'es-PA' | 'es-PY' | 'es-PE' | 'es-PR' | 'es-DO' | 'es-UY' | 'es-VE' | 'eu-ES' | 'fil-PH' | 'fr-CA' | 'fr-FR' | 'gl-ES' | 'ka-GE' | 'gu-IN' | 'hr-HR' | 'zu-ZA' | 'is-IS' | 'it-IT' | 'jv-ID' | 'kn-IN' | 'km-KH' | 'lo-LA' | 'lv-LV' | 'lt-LT' | 'hu-HU' | 'ml-IN' | 'mr-IN' | 'nl-NL' | 'ne-NP' | 'nb-NO' | 'pl-PL' | 'pt-BR' | 'pt-PT' | 'ro-RO' | 'si-LK' | 'sk-SK' | 'sl-SI' | 'su-ID' | 'sw-TZ' | 'sw-KE' | 'fi-FI' | 'sv-SE' | 'ta-IN' | 'ta-SG' | 'ta-LK' | 'ta-MY' | 'te-IN' | 'vi-VN' | 'tr-TR' | 'ur-PK' | 'ur-IN' | 'el-GR' | 'bg-BG' | 'ru-RU' | 'sr-RS' | 'uk-UA' | 'he-IL' | 'ar-IL' | 'ar-JO' | 'ar-AE' | 'ar-BH' | 'ar-DZ' | 'ar-SA' | 'ar-IQ' | 'ar-KW' | 'ar-MA' | 'ar-TN' | 'ar-OM' | 'ar-PS' | 'ar-QA' | 'ar-LB' | 'ar-EG' | 'fa-IR' | 'hi-IN' | 'th-TH' | 'ko-KR' | 'zh-TW' | 'yue-Hant-HK' | 'ja-JP' | 'zh-HK' | 'zh'))` |   | `false` |  
`autoresume` | Autmaticaly resume regognition after receiving results | `boolean()` | `false` | `false` | `supported`
`enable_automatic_punctuation` | If 'true', adds punctuation to recognition result hypotheses. This feature is only available in select languages | `boolean()` | `false` | `false` | `supported`
`enable_word_time_offsets` | If true, the top result includes a list of words and the start and end time offsets (timestamps) for those words. If false, no word-level time offset information is returned. | `boolean()` | `false` | `false` | `supported`
`format` | Which output format need to use for ASR results | `string('plain' | 'json')` | `plain` | `false` | `supported`
`interim_results` | If true, interim results (tentative hypotheses) may be returned as they become available (these interim results are indicated with the is_final=false flag). If false or omitted, only is_final=true result(s) are returned. | `boolean()` | `false` | `false` | `supported`
`language_code` | The language of the supplied audio as a BCP-47 language tag | `string('af-ZA' | 'am-ET' | 'hy-AM' | 'az-AZ' | 'id-ID' | 'ms-MY' | 'bn-BD' | 'bn-IN' | 'ca-ES' | 'cs-CZ' | 'da-DK' | 'de-DE' | 'en-AU' | 'en-CA' | 'en-GH' | 'en-GB' | 'en-IN' | 'en-IE' | 'en-KE' | 'en-NZ' | 'en-NG' | 'en-PH' | 'en-SG' | 'en-ZA' | 'en-TZ' | 'en-US' | 'es-AR' | 'es-BO' | 'es-CL' | 'es-CO' | 'es-CR' | 'es-EC' | 'es-SV' | 'es-ES' | 'es-US' | 'es-GT' | 'es-HN' | 'es-MX' | 'es-NI' | 'es-PA' | 'es-PY' | 'es-PE' | 'es-PR' | 'es-DO' | 'es-UY' | 'es-VE' | 'eu-ES' | 'fil-PH' | 'fr-CA' | 'fr-FR' | 'gl-ES' | 'ka-GE' | 'gu-IN' | 'hr-HR' | 'zu-ZA' | 'is-IS' | 'it-IT' | 'jv-ID' | 'kn-IN' | 'km-KH' | 'lo-LA' | 'lv-LV' | 'lt-LT' | 'hu-HU' | 'ml-IN' | 'mr-IN' | 'nl-NL' | 'ne-NP' | 'nb-NO' | 'pl-PL' | 'pt-BR' | 'pt-PT' | 'ro-RO' | 'si-LK' | 'sk-SK' | 'sl-SI' | 'su-ID' | 'sw-TZ' | 'sw-KE' | 'fi-FI' | 'sv-SE' | 'ta-IN' | 'ta-SG' | 'ta-LK' | 'ta-MY' | 'te-IN' | 'vi-VN' | 'tr-TR' | 'ur-PK' | 'ur-IN' | 'el-GR' | 'bg-BG' | 'ru-RU' | 'sr-RS' | 'uk-UA' | 'he-IL' | 'ar-IL' | 'ar-JO' | 'ar-AE' | 'ar-BH' | 'ar-DZ' | 'ar-SA' | 'ar-IQ' | 'ar-KW' | 'ar-MA' | 'ar-TN' | 'ar-OM' | 'ar-PS' | 'ar-QA' | 'ar-LB' | 'ar-EG' | 'fa-IR' | 'hi-IN' | 'th-TH' | 'ko-KR' | 'zh-TW' | 'yue-Hant-HK' | 'ja-JP' | 'zh-HK' | 'zh')` | `en-US` | `true` |  
`max_alternatives` | Maximum number of recognition hypotheses to be returned | `integer()` | `1` | `false` | `supported`
`metadata.audio_topic` | Description of the content | `string()` |   | `false` |  
`metadata.interaction_type` | Use case categories that the audio recognition request can be described by | `string('INTERACTION_TYPE_UNSPECIFIED' | 'DISCUSSION' | 'PRESENTATION' | 'PHONE_CALL' | 'VOICEMAIL' | 'PROFESSIONALLY_PRODUCED' | 'VOICE_SEARCH' | 'VOICE_COMMAND' | 'DICTATION')` |   | `false` |  
`metadata.microphone_distance` | Enumerates the types of capture settings describing an audio file | `string('MICROPHONE_DISTANCE_UNSPECIFIED' | 'NEARFIELD' | 'MIDFIELD' | 'FARFIELD')` |   | `false` |  
`metadata.original_mime_type` | Mime type of the original audio file | `string()` |   | `false` |  
`metadata.recording_device_name` | The device used to make the recording | `string()` |   | `false` |  
`metadata.recording_device_type` | The type of device the speech was recorded with | `string('RECORDING_DEVICE_TYPE_UNSPECIFIED' | 'SMARTPHONE' | 'PC' | 'PHONE_LINE' | 'VEHICLE' | 'OTHER_OUTDOOR_DEVICE' | 'OTHER_INDOOR_DEVICE')` |   | `false` |  
`metadata` |   | `object()` |   | `false` |  
`model` | Which model to select for the given request. | `string('command_and_search' | 'phone_call' | 'video' | 'default')` |   | `false` | `supported`
`profanity_filter` | If set to true, the server will attempt to filter out profanities, replacing all but the initial character in each filtered word with asterisks, e.g. 'f***'. If set to false or omitted, profanities won't be filtered out. | `boolean()` | `false` | `false` | `supported`
`provider` |   | `string('google')` |   | `true` |  
`single_utterance` | If false or omitted, the recognizer will perform continuous recognition (continuing to wait for and process audio even if the user pauses speaking) until the client closes the input stream (gRPC API) or until the maximum time limit has been reached. If true, the recognizer will detect a single spoken utterance. When it detects that the user has paused or stopped speaking, it will return an END_OF_SINGLE_UTTERANCE event and cease recognition. | `boolean()` | `false` | `false` | `supported`
`speech_contexts.[]` |   | `string(1..)` |   | `false` |  
`speech_contexts` | Provides 'hints' to the speech recognizer to favor specific words and phrases in the results | `array(string(1..))` |   | `false` |  
`use_enhanced` | Set to true to use an enhanced model for speech recognition | `boolean()` | `false` | `false` | `supported`



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/speech_config

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/speech_config
```

```json
{
    "page_size": {CONFIG_ID_COUNT},
    "data": [
        {
            "id": "{SPEECH_CONFIG_ID}",
            "name": "{CONFIG_NAME}",
        },
        {
            "id": "{SPEECH_CONFIG_ID}",
            "name": "{CONFIG_NAME}",
        }
    ],
    "revision": "{REVISION}",
    "timestamp": "{TIMESTAMP}",
    "version": "{APP_VERSION}",
    "node": "{NODE_ID}",
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/speech_config

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"name":"my_config"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/speech_config
```

```json
{
    "data": {
        "name": "my_config"
    },
    "revision": "{REVISION}",
    "timestamp": "{TIMESTAMP}",
    "version": "{APP_VERSION}",
    "node": "{NODE_ID}",
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```


## Create speech config with google asr profile

> PUT /v2/accounts/{ACCOUNT_ID}/speech_config

Request content inside `filename.json`

```json
{
    "data": {
        "name": "my_config"
        "asr_config": {
            "model": "phone_call",
            "use_enhanced": true,
            "provider": "google",
            "autoresume": false,
            "enable_automatic_punctuation": false,
            "enable_word_time_offsets": true,
            "format": "plain",
            "interim_results": false,
            "language_code": "en-US",
            "max_alternatives": 1,
            "profanity_filter": false,
            "single_utterance": false
        }
    }
}
```

Sending request

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d @filename.json \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/speech_config
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/speech_config/{SPEECH_CONFIG_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/speech_config/{SPEECH_CONFIG_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/speech_config/{SPEECH_CONFIG_ID}

Request content inside `filename.json`

```json
{
    "data": {
        "name": "my_config"
        "asr_config": {
            "model": "phone_call",
            "use_enhanced": true,
            "provider": "google",
            "autoresume": false,
            "enable_automatic_punctuation": false,
            "enable_word_time_offsets": true,
            "format": "plain",
            "interim_results": false,
            "language_code": "en-US",
            "max_alternatives": 1,
            "profanity_filter": false,
            "single_utterance": false
        }
    }
}
```

Sending request

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d @filename.json \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/speech_config/{SPEECH_CONFIG_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/speech_config/{SPEECH_CONFIG_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"some_key":"some_value"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/speech_config/{SPEECH_CONFIG_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/speech_config/{SPEECH_CONFIG_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/speech_config/{SPEECH_CONFIG_ID}
```

## Fetch the account doc

> GET /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "billing_mode": "manual",
        "call_restriction": {},
        "caller_id": {},
        "created": 63621662701,
        "dial_plan": {},
        "enabled": true,
        "id": "{ACCOUNT_ID}",
        "is_reseller": false,
        "language": "en-us",
        "music_on_hold": {},
        "name": "child account",
        "preflow": {},
        "realm": "aeac33.sip.2600hz.com",
        "reseller_id": "undefined",
        "ringtones": {},
        "superduper_admin": false,
        "timezone": "America/Los_Angeles",
        "wnm_allow_additions": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Patch the account doc

> PATCH /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"some_key":"some_value"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "billing_mode": "manual",
        "call_restriction": {},
        "caller_id": {},
        "created": 63621662701,
        "dial_plan": {},
        "enabled": true,
        "id": "{ACCOUNT_ID}",
        "is_reseller": false,
        "language": "en-us",
        "music_on_hold": {},
        "name": "child account",
        "preflow": {},
        "realm": "aeac33.sip.2600hz.com",
        "reseller_id": "undefined",
        "ringtones": {},
        "some_key":"some_value",
        "superduper_admin": false,
        "timezone": "America/Los_Angeles",
        "wnm_allow_additions": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Change the account doc

> POST /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {"billing_mode": "manual","call_restriction": {},"caller_id": {},"created": 63621662701,"dial_plan": {},"enabled": true,"is_reseller": false,"language": "en-us","music_on_hold": {},"name": "child account","preflow": {},"realm": "aeac33.sip.2600hz.com","reseller_id": "undefined","ringtones": {},"some_key":"some_value","superduper_admin": false,"timezone": "America/Los_Angeles","wnm_allow_additions": false}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "billing_mode": "manual",
        "call_restriction": {},
        "caller_id": {},
        "created": 63621662701,
        "dial_plan": {},
        "enabled": true,
        "id": "{ACCOUNT_ID}",
        "is_reseller": false,
        "language": "en-us",
        "music_on_hold": {},
        "name": "child account",
        "preflow": {},
        "realm": "aeac33.sip.2600hz.com",
        "reseller_id": "undefined",
        "ringtones": {},
        "some_key":"some_value",
        "superduper_admin": false,
        "timezone": "America/Los_Angeles",
        "wnm_allow_additions": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Create a new child account

Puts the created account under `{ACCOUNT_ID}`

> PUT /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"name":"child account"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "billing_mode": "manual",
        "call_restriction": {},
        "caller_id": {},
        "created": 63621662701,
        "dial_plan": {},
        "enabled": true,
        "id": "{CHILD_ACCOUNT_ID}",
        "is_reseller": false,
        "language": "en-us",
        "music_on_hold": {},
        "name": "child account",
        "preflow": {},
        "realm": "aeac33.sip.2600hz.com",
        "reseller_id": "undefined",
        "ringtones": {},
        "superduper_admin": false,
        "timezone": "America/Los_Angeles",
        "wnm_allow_additions": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Fetch the parent account IDs

> GET /v2/accounts/{ACCOUNT_ID}/parents

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/parents
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "{PARENT_ACCOUNT_ID}",
            "name": "{PARENT_ACCOUNT_NAME}"
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Fetch an account's ancestor tree

> GET /v2/accounts/{ACCOUNT_ID}/tree

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tree
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "{PARENT_ACCOUNT_ID}",
            "name": "{PARENT_ACCOUNT_NAME}"
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Fetch the account's API key

The API key is used by the `api_auth` API to obtain an `auth_token`. This is intended for use by applications talking to kazoo and provides a mechanism for authentication that does not require storing a username and password in the application. The API key can be obtained via the accounts API's endpoint `api_key`.

> GET /v2/accounts/{ACCOUNT_ID}/api_key

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
     http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/api_key
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "api_key": "{API_KEY}"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Re-create the account's API key

If you think that your account's API key might be exposed you can create a new one with `api_key` endpoint. Issuing a `PUT` request to this endpoint will generates a new API key for the account and will returned it in response.

> PUT /v2/accounts/{ACCOUNT_ID}/api_key

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
     http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/api_key
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "api_key": "{API_KEY}"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Fetch sibling accounts

By default a user account under an admin/reseller account can view all the other accounts under that reseller. If you would like current account only will be able to query its child accounts' sibling and not other accounts then set `allow_sibling_listing` in `system_config/crossbar.accounts` to `false`. Admin account can unrestrictedly list siblings.

> GET /v2/accounts/{ACCOUNT_ID}/siblings

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/siblings
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "descendants_count": 1,
            "id": "{ACCOUNT_ID}",
            "name": "{ACCOUNT_NAME}",
            "realm": "{ACCOUNT_REALM}"
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "start_key": "",
    "status": "success"
}
```

## Fetch all descendants of an account

This will include children, grandchildren, etc

> GET /v2/accounts/{ACCOUNT_ID}/descendants

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/descendants
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "{CHILD_ACCOUNT}",
            "name": "{CHILD_NAME}",
            "realm": "{CHILD_REALM}",
            "tree": [
                "{ACCOUNT_ID}"
            ]
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "start_key": "",
    "status": "success"
}
```

## Fetch immediate children of an account

> GET /v2/accounts/{ACCOUNT_ID}/children

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/children
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "{CHILD_ACCOUNT}",
            "name": "{CHILD_NAME}",
            "realm": "{CHILD_REALM}",
            "tree": [
                "{ACCOUNT_ID}"
            ]
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "start_key": "",
    "status": "success"
}
```

## Demote a reseller

Requires superduper admin auth token

> DELETE /v2/accounts/{ACCOUNT_ID}/reseller

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/reseller
```

## Promote a reseller

Requires superduper admin auth token

> PUT /v2/accounts/{ACCOUNT_ID}/reseller

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/reseller
```


## Move an account

An account can only be moved by a "superduper_admin" or  if enabled by anyone above the desired account.

You can enable that feature by editing the document `crossbar.accounts` in your `system_config` database and set the value to `tree`.

Key | Value | Description
--- | ----- | -----------
`allow_move` | enum("tree", "superduper_admin") | Who can move a sub-account

> POST /v2/accounts/{ACCOUNT_ID}/move

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"to": "{ACCOUNT_ID_DESTINATION}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/move
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "billing_mode": "manual",
        "call_restriction": {},
        "caller_id": {},
        "created": 63621662701,
        "dial_plan": {},
        "enabled": true,
        "id": "{ACCOUNT_ID}",
        "is_reseller": false,
        "language": "en-us",
        "music_on_hold": {},
        "name": "child account",
        "preflow": {},
        "realm": "aeac33.sip.2600hz.com",
        "reseller_id": "undefined",
        "ringtones": {},
        "superduper_admin": false,
        "timezone": "America/Los_Angeles",
        "wnm_allow_additions": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
