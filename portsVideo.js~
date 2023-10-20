const { nowInSec, SkyWayAuthToken, SkyWayContext, SkyWayRoom, SkyWayStreamFactory, uuidV4 } = skyway_room;

// Elmアプリケーションを開始します
var app = Elm.MainOct09.init({
    node: document.getElementById('myapp')
});



const token = new SkyWayAuthToken({
  jti: uuidV4(),
  iat: nowInSec(),
  exp: nowInSec() + 60 * 60 * 24,
  scope: {
    app: {
      id: '489b7100-9ce2-4b50-ac74-1b143ef84667',
      turn: true,
      actions: ['read'],
      channels: [
        {
          id: '*',
          name: '*',
          actions: ['write'],
          members: [
            {
              id: '*',
              name: '*',
              actions: ['write'],
              publication: {
                actions: ['write'],
              },
              subscription: {
                actions: ['write'],
              },
            },
          ],

          sfuBots: [
            {
              actions: ['write'],
              forwardings: [
                {
                  actions: ['write'],
                },
              ],
            },
          ],
        },
      ],
    },
  },
}).encode('cwBzLkd/0a1vrqUzIegWO5Q09NAajW4CPF7qCCABJ/E=');



(async () => {

    const data = await SkyWayStreamFactory.createDataStream();
	

    const { audio, video } = await SkyWayStreamFactory.createMicrophoneAudioAndCameraStream(); // 2

    
    app.ports.sendXY.subscribe(function(p) {
	//console.log(p)
	data.write({class:"ami",pos:p});
    });
    app.ports.sendKingyo.subscribe(function(kingyos) {
	//console.log(kingyos)
	data.write({class:"kingyo", kingyos:kingyos});
    });
    app.ports.caught.subscribe(function(info) {
	//console.log(info)
	data.write({class:"caught",
		    kingyos:info.kingyos,
		    num:info.num,
		    id:info.id,
		    points:info.points
		   });
    });
    app.ports.join.subscribe(async (room) => {
	console.log(room)
	if (room === '') return;

	const context = await SkyWayContext.Create(token);
	const channel = await SkyWayRoom.FindOrCreate(context, {
	    type: 'p2p',
	    name: room
	});
	if (channel.members.length >= 4) {
	    return;
	}
	const me = await channel.join();

	app.ports.skywayId.send({id:me.id,num:channel.members.length});
	console.log(channel.members.length);
	await me.publish(data);
	await me.publish(video);
	const localVideo = document.getElementById('player1');
	video.attach(localVideo); // 3
	await localVideo.play(); // 4
	let videoNum = 1;

	const subscribeAndAttach = async (publication) => {
	    if (publication.publisher.id === me.id) return;

            const { stream } = await me.subscribe(publication.id);
	    let newMedia; // 3-2-2
	    console.log(publication.publisher.id);
	    //switch (stream.track.kind) {
	    switch (stream.contentType) {
	    case 'video':
		videoNum += 1;
		console.log("video");
		newMedia = document.createElement('video');
		newMedia.playsInline = true;
		newMedia.style.width = "100px";
		newMedia.style.height = "100px";
		newMedia.autoplay = true;
		stream.attach(newMedia); // 3-2-3
		playerNumber = channel.members.length;
		console.log('playervideo'+videoNum);
		const player = document.getElementById('playervideo'+videoNum);
		player.appendChild(newMedia);
		break;
	    case 'data':
		console.log("data");
		stream.onData.add((newdata) => {
		    if (newdata.class == "ami"){
			app.ports.moveInfo.send(newdata.pos);
		    }
		    else if (newdata.class == "kingyo"){
			app.ports.kingyoInfo.send(newdata.kingyos);
		    }
		    else if (newdata.class == "caught"){
			console.log(newdata);
			app.ports.kingyoCaught.send(
			    {kingyos:newdata.kingyos,
			     id:newdata.id,
			     points:newdata.points
			    });
		    }
		});
		break;
	    default:
		return;
	    }
	};
	channel.publications.forEach(subscribeAndAttach);
	channel.onStreamPublished.add((e) => subscribeAndAttach(e.publication));
    });
    
})();
