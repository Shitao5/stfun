#' Send a message to DingTalk robot
#'
#' This function sends a message to a DingTalk robot using the provided webhook URL.
#'
#' @param message The message content to send.
#' @param include_timestamp Whether to include the current timestamp in the message. Default is \code{FALSE}.
#' @param at_mobiles A character vector of mobile numbers to mention in the message. Default is \code{NULL}.
#' @param at_all Whether to mention all users in the message. Default is \code{FALSE}.
#' @keywords dingtalk message
#' @importFrom httr POST add_headers http_status
#' @importFrom jsonlite toJSON
#' @importFrom cli cli_alert_success cli_alert_danger
#' @export
#'
#' @examples
#' \dontrun{
#' options(webhook_ding = "https://oapi.dingtalk.com/robot/send?access_token=YOUR_ACCESS_TOKEN")
#' send_message_ding("Hello, World!")
#' send_message_ding("Code execution completed.", include_timestamp = TRUE,
#'  at_mobiles = c("1234567890", "0987654321"))
#' send_message_ding("Code execution completed.", include_timestamp = TRUE, at_all = TRUE)
#' }
#'
#' @seealso
#' \url{https://open.dingtalk.com/document/robots/custom-robot-access}
send_message_ding <- function(message, include_timestamp = FALSE, at_mobiles = NULL, at_all = FALSE) {
  # Get the webhook from the global options
  webhook <- getOption("webhook_ding")

  # If include_timestamp is TRUE, add the current timestamp to the message
  if (include_timestamp) {
    timestamp_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message <- paste0("Current timestamp:", timestamp_str, "\n", message)
  }

  # Construct the mentioned users object
  mentioned_users <- list()
  if (!is.null(at_mobiles)) {
    # To avoid errors when mentioning only one person with the "@" symbol, add a non-existent phone number.
    at_mobiles <- c(at_mobiles, "1")
    mentioned_users$atMobiles <- at_mobiles
  }
  if (at_all) {
    mentioned_users$isAtAll <- TRUE
  }

  # Construct the message payload
  payload <- list(
    msgtype = "text",
    text = list(
      content = message
    ),
    at = mentioned_users
  )

  # Convert the message content to UTF-8 encoded JSON string
  payload_json <- toJSON(payload, auto_unbox = TRUE, force = TRUE, encode = "utf-8")

  # Send a POST request to the DingTalk robot
  response <- POST(
    url = webhook,
    body = payload_json,
    add_headers("Content-Type" = "application/json;charset=utf-8")
  )

  # Check if the request was successful and provide execution status
  if (http_status(response)$category == "Success") {
    cli_alert_success("Message sent successfully!")
  } else {
    cli_alert_danger("Failed to send message.")
  }
}


#' Send a message to Enterprise WeChat
#'
#' This function sends a message to an Enterprise WeChat robot using the provided webhook URL.
#'
#' @param message The message content to be sent.
#' @param include_timestamp Logical indicating whether to include the current timestamp in the message. Default is \code{FALSE}.
#' @param at_mobiles A character vector of mobile numbers to be mentioned in the message. Default is \code{NULL}.
#' @param at_all Logical indicating whether to mention all members. Default is \code{FALSE}.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' options(webhook_wx = "https://qyapi.weixin.qq.com/robot/send?access_token=YOUR_ACCESS_TOKEN")
#' send_message_wx("Code execution completed.", include_timestamp = TRUE, at_mobiles = "1234567890")
#' send_message_wx("Code execution completed.", include_timestamp = TRUE, at_all = TRUE)
#' }
#'
#' @importFrom httr POST add_headers http_status
#' @importFrom jsonlite toJSON
#' @importFrom cli cli_alert_success cli_alert_danger
#'
#' @export
send_message_wx <- function(message, include_timestamp = FALSE, at_mobiles = NULL, at_all = FALSE) {
  # Get the webhook from the global options
  webhook <- getOption("webhook_wx")

  # If include_timestamp is TRUE, add the current timestamp to the message
  if (include_timestamp) {
    timestamp_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    message <- paste0("Current timestamp:", timestamp_str, "\n", message)
  }

  # Construct the mentioned_list and mentioned_mobile_list as lists
  # mentioned_list <- ifelse(!is.null(at_users), at_users, list())
  mentioned_mobile_list <- c(ifelse(!is.null(at_mobiles), at_mobiles, list()), ifelse(at_all, "@all", list()))

  # Construct the message payload
  payload <- list(
    msgtype = "text",
    text = list(
      content = message,
      # mentioned_list = mentioned_list,
      mentioned_mobile_list = mentioned_mobile_list
    )
  )

  # Convert the message content to UTF-8 encoded JSON string
  payload_json <- toJSON(payload, auto_unbox = TRUE, force = TRUE, encode = "utf-8")

  # Send a POST request to the Enterprise WeChat robot
  response <- POST(
    url = webhook,
    body = payload_json,
    add_headers("Content-Type" = "application/json;charset=utf-8")
  )

  # Check if the request was successful and provide execution status
  if (http_status(response)$category == "Success") {
    cli_alert_success("Message sent successfully!")
  } else {
    cli_alert_danger("Failed to send message.")
  }
}
