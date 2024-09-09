use super::documents::DocumentIndex;
use lsp_types::notification::*;

pub fn handle(notification: lsp_server::Notification, documents: &mut DocumentIndex) {
  match notification.method.as_str() {
    DidOpenTextDocument::METHOD => {
      let params = get_params::<DidOpenTextDocument>(notification);

      documents.open(params.text_document.uri, params.text_document.text);
    }
    DidCloseTextDocument::METHOD => {
      let params = get_params::<DidCloseTextDocument>(notification);

      documents.close(&params.text_document.uri);
    }
    DidChangeTextDocument::METHOD => {
      let params = get_params::<DidChangeTextDocument>(notification);

      let existing_file = documents.get(&params.text_document.uri);

      for change in params.content_changes {
        assert!(change.range.is_none(), "don't support partial updates");

        existing_file.update(change.text);
      }
    }

    _ => eprintln!("Unknown Notification:\n\t{notification:?}"),
  };
}

fn get_params<N: lsp_types::notification::Notification>(
  notification: lsp_server::Notification,
) -> N::Params {
  notification.extract(N::METHOD).unwrap()
}
