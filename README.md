
# emqx-ct-helpers

EMQ X Common Test Helpers

## Erlc Options

```
TEST_ERLC_OPTS += +'{parse_transform, emqx_ct_transform}'
TEST_ERLC_OPTS += +'{mock_module, emqx_broker, emqx_ct_broker}'
TEST_ERLC_OPTS += +'{mock_module, emqx_access_control, emqx_ct_access_control}'
```

## License

Apache License Version 2.0

## Author

EMQ X Team.

