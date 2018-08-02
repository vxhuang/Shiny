// Dean Attali, July 2015

app = function() {
  // the message to share
  var payload = {
    picture : 'http://deanattali.com/img/deanimg.jpeg',
    link : 'http://54.210.79.190',
    title : 'My Life Path',
    caption : 'This website generates a map of your life path. ',
    description : 'Hi, this website helps you generate a map with a trajectory of your life on it. '
  };
  
  // facebook access token if the user is logged in and sharing permissions
  var fbToken = null;

  return {
    
    init : function() {
      
      // initialize facebook JDK
      (function(d, s, id) {
        var js, fjs = d.getElementsByTagName(s)[0];
        if (d.getElementById(id)) return;
        js = d.createElement(s);
        js.id = id;
        js.src = 'http://connect.facebook.net/en_US/sdk.js';
        fjs.parentNode.insertBefore(js, fjs);
      }(document, 'script', 'facebook-jssdk'));  
      window.fbAsyncInit = function() {
        FB.init({
          appId: '298487377558708', //  Facebook app ID (works when app is at URL fuf.me)
          cookie: true, 
          version: 'v3.0'
        });
        
        // on page load, if the user is logged in, check for sharing permissions
        // and store the access token
        FB.getLoginStatus(function(response) {
          if (response.status === 'connected') {
            FB.api("me/permissions", function (resp) {
              if (resp.data) {
                for(var i = 0; i < resp.data.length; i++) {
                  if (resp.data[i].permission == "publish_actions" &&
                      resp.data[i].status == "granted")
                  {
                    fbToken = response.authResponse.accessToken;
                  }
                }
              }
            });
          }
        });
      };      
      
      // register click event on facebook share button
      $('#fbShareBtn').click(app.facebookShare);
      
      // register click event on facebook share base64 image button
      $('#fbShare64Btn').click(app.facebookShare64Click);
    },

    // share image on facebook    
    facebookShare : function() {
      var params = {
          method : 'feed',
          picture : payload.picture,
          link : payload.link,
          name : payload.title,
          caption : payload.caption,
          description : payload.description
      }; 
      FB.ui(params, function(response) {
        if (response && response.post_id) {
          alert('Facebook share success!');
          console.log(response);
        } else {
          alert('Facebook share error'); 
          console.log(response);
        }
      });    
    },
    
    // user clicked on sharing the base64 image
    facebookShare64Click : function() {
      // if there is an access token, call the function to share the image
      if (fbToken !== null) {
        app.facebookShare64();
      } else {
        // if the user isn't logged in or doesn't have sharing permissions,
        // prompt for it and then store the access token and attempt to sharethe image
        FB.login(function(response) {
          if(response.status == "connected") {
            var perms = response.authResponse.grantedScopes.split(",");
            if ($.inArray("publish_actions", perms) > -1) {
              fbToken = response.authResponse.accessToken;
              app.facebookShare64();
              return;
            }
          }
          alert("Error - app does not have permission to share posts");
        }, {
          scope : 'publish_actions',
          return_scopes : true
        });                
      }
    },

    // share a base64 encoded image to facebook
    facebookShare64 : function() {
      var base64img  = $("#plot").find("img").attr('src');
      var blob = dataURItoBlob(base64img);
      var fd = new FormData();
      fd.append("access_token", fbToken);
      fd.append("source", blob);
      fd.append("message", payload.title);
      $.ajax({
        url : "https://graph.facebook.com/me/photos?access_token=" + fbToken,
        type : "POST",
        data : fd,
        processData : false,
        contentType : false,
        cache : false,
        success : function(data){
          alert('Facebook share success!');
          console.log(data);
        },
        error : function(shr, status, data){
          alert('Facebook share error'); 
          console.log(data);
        }
      });
    }
  }; 
}();

$(function () { app.init(); });

function dataURItoBlob(dataURI) {
  var byteString = atob(dataURI.split(',')[1]);
  var ab = new ArrayBuffer(byteString.length);
  var ia = new Uint8Array(ab);
  for (var i = 0; i < byteString.length; i++) {
    ia[i] = byteString.charCodeAt(i);
  }
  return new Blob([ab], { type: 'image/png' });
}