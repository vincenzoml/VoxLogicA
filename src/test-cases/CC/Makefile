.PHONY: CPU GPU clean

# CPU:
# 	rm -rf CPU && mkdir CPU && cp test.png test.imgql CPU && cd CPU && ../../../../VoxLogicA/src/bin/release/netcoreapp3.1/linux-x64/VoxLogicA test.imgql

GPU:
	(cd ../.. && make) && mkdir -p GPU && cp ../../kernel.cl test.png test.imgql GPU && cd GPU && ../../../bin/release/netcoreapp3.1/linux-x64/VoxLogicA test.imgql

clean:
	rm -rf CPU GPU