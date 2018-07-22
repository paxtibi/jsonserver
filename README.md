Config file example:

{
  "config": {
    "port": 3000
  },
  "routers": [
    {
      "method": "GET",
      "route": "/api"
    },
    {
      "method": "GET",
      "route": "/user/list",
      "dataset":"user.json"
    },
    {
      "method": "GET",
      "route": "/user/:id/info",
      "dataset":"user.json"
    },
    {
      "method": "GET",
      "route": "/user/:id/last-login",
      "dataset":"user.json"
    },
    {
      "method": "PUT",
      "route": "/user/create",
      "dataset":"user.json"
    },
    {
      "method": "POST",
      "route": "/user/:id/update",
      "dataset":"user.json"
    },
    {
      "method": "DELETE",
      "route": "/user/:id",
      "dataset":"user.json"
    }
  ]
}
