--redis-cli --eval adduser.lua "user:justin"  "salt"   "HASHED"
--                             KEYS[1]        KEYS[2]  KEYS[3]
local user   = redis.call("EXISTS", KEYS[1])
if user == 0 then
    local createid = redis.call("INCR", "server:usercounter")
    local userid = "userid:" .. createid
    redis.call("SET",  KEYS[1], createid)
    redis.call("HSET", userid, "salt", KEYS[2], "password", KEYS[3])
    return {createid}
else return {} end
