# Source state attributes

The following prefixes and suffixes are special, and are collectively referred to as "attributes":

| Prefix    | Effect                                                                              |
|-----------|-------------------------------------------------------------------------------------|
| after_    | Run script after updating the destination                                           |
| before_   | Run script before updating the destination                                          |
| modify_   | Treat the contents as a script that modifies an existing file                       |
| once_     | Only run the script if its contents have not been run before                        |
| onchange_ | Only run the script if its contents have not been run before with the same filename |
| run_      | Treat the contents as a script to run                                               |
