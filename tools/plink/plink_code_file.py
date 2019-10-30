def exec_after_process(app, inp_data, out_data, param_dict, tool, stdout, stderr):
    for name, data in out_data.items():
        if data.has_data():
            data.visible = True
            app.model.context.add(data)
    app.model.context.flush()
