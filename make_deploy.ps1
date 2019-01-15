& sh ./release_unzip_tar.sh
Compress-Archive -Path deploy\node1-ipro-0.0.1 -CompressionLevel Fastest -DestinationPath deploy\node1-ipro-0.0.1.zip
Compress-Archive -Path deploy\node2-ipro-0.0.1 -CompressionLevel Fastest -DestinationPath deploy\node2-ipro-0.0.1.zip