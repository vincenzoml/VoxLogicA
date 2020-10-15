layoutName = 'Red'
imagePathPattern = '/tmp/image-%03d.png'
steps = 34

widget = slicer.app.layoutManager().sliceWidget(layoutName)
view = widget.sliceView()
logic = widget.sliceLogic()
bounds = [0,]*6
logic.GetSliceBounds(bounds)

for step in range(steps):
    offset = bounds[4] + step/(1.*steps) * (bounds[5]-bounds[4])
    logic.SetSliceOffset(offset)
    view.forceRender()
    image = qt.QPixmap.grabWidget(view).toImage()
    image.save(imagePathPattern % step)
