docker:
	docker build -t lem-rooms-client .

run:
	docker run --env-file .env -e DOCKER=1 --mount source=lem-rooms-client-data,target=/root -it --rm --name lem-rooms-client -p 50000:50000 lem-rooms-client

clean:
	docker container rm -f lem-rooms-client
	docker volume rm -f lem-rooms-client-data
	docker image rm -f lem-rooms-client
