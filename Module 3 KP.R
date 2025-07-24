
# Define PolicyHolder class
PolicyHolder <- setRefClass(
  "PolicyHolder",
  fields = list(
    holder_id = "character",
    name = "character",
    email = "character",
    active = "logical",
    policies = "list"
  ),
  methods = list(
    initialize = function(holder_id, name, email) {
      holder_id <<- holder_id
      name <<- name
      email <<- email
      active <<- TRUE
      policies <<- list()
    },
    
    suspend = function() {
      active <<- FALSE
    },
    
    activate = function() {
      active <<- TRUE
    },
    
    add_policy = function(policy) {
      policies <<- c(policies, list(policy))
    },
    
    show = function() {
      status <- if (active) "Active" else "Suspended"
      cat(sprintf("%s (%s)\n", name, status))
    }
  )
)

# Define PolicyProduct class
PolicyProduct <- setRefClass(
  "PolicyProduct",
  fields = list(
    product_id = "character",
    name = "character",
    premium = "numeric",
    coverage = "character",
    active = "logical"
  ),
  methods = list(
    initialize = function(product_id, name, premium, coverage) {
      product_id <<- product_id
      name <<- name
      premium <<- premium
      coverage <<- coverage
      active <<- TRUE
    },
    
    suspend = function() {
      active <<- FALSE
    },
    
    activate = function() {
      active <<- TRUE
    },
    
    show = function() {
      status <- if (active) "Active" else "Suspended"
      cat(sprintf("%s - Premium: $%.2f, Coverage: %s (%s)\n", name, premium, coverage, status))
    }
  )
)

# Define Payment class
Payment <- setRefClass(
  "Payment",
  fields = list(
    payment_id = "character",
    holder_id = "character",
    amount = "numeric",
    due_date = "Date",
    date_paid = "Date",
    status = "character"
  ),
  methods = list(
    initialize = function(payment_id, holder_id, amount, due_date, date_paid = NA) {
      payment_id <<- payment_id
      holder_id <<- holder_id
      amount <<- amount
      due_date <<- as.Date(due_date)
      date_paid <<- if (!is.na(date_paid)) as.Date(date_paid) else as.Date(NA)
      status <<- if (!is.na(date_paid)) "Paid" else "Pending"
    },
    
    mark_paid = function(date_paid = Sys.Date()) {
      date_paid <<- as.Date(date_paid)
      status <<- "Paid"
    },
    
    is_overdue = function() {
      return(status == "Pending" && Sys.Date() > due_date)
    },
    
    show = function() {
      cat(sprintf("Payment %s - $%.2f - %s (Due: %s)\n",
                  payment_id, amount, status, format(due_date, "%Y-%m-%d")))
    }
  )
)

# Define PolicyManagementSystem class
PolicyManagementSystem <- setRefClass(
  "PolicyManagementSystem",
  fields = list(
    policyholders = "list",
    products = "list",
    payments = "list"
  ),
  methods = list(
    initialize = function() {
      policyholders <<- list()
      products <<- list()
      payments <<- list()
    },
    
    add_policyholder = function(holder_id, name, email) {
      policyholders[[holder_id]] <<- PolicyHolder$new(holder_id = holder_id, name = name, email = email)
    },
    
    suspend_policyholder = function(holder_id) {
      if (!is.null(policyholders[[holder_id]])) {
        policyholders[[holder_id]]$suspend()
      }
    },
    
    register_product = function(product_id, name, premium, coverage) {
      products[[product_id]] <<- PolicyProduct$new(product_id = product_id, name = name, premium = premium, coverage = coverage)
    },
    
    assign_policy = function(holder_id, product_id) {
      if (!is.null(policyholders[[holder_id]]) && !is.null(products[[product_id]])) {
        policyholders[[holder_id]]$add_policy(products[[product_id]])
      }
    },
    
    suspend_product = function(product_id) {
      if (!is.null(products[[product_id]])) {
        products[[product_id]]$suspend()
      } else {
        cat("Product not found.\n")
      }
    },
    
    reactivate_product = function(product_id) {
      if (!is.null(products[[product_id]])) {
        products[[product_id]]$activate()
      } else {
        cat("Product not found.\n")
      }
    },
    
    show_all_assigned_policies = function() {
      for (holder_id in names(policyholders)) {
        holder <- policyholders[[holder_id]]
        cat(sprintf("\nPolicyholder: %s (ID: %s)\n", holder$name, holder_id))
        if (length(holder$policies) > 0) {
          for (policy in holder$policies) {
            policy$show()
          }
        } else {
          cat("  No policies assigned.\n")
        }
      }
    },
    
    record_payment = function(payment_id, holder_id, amount) {
      payment <- Payment$new(payment_id = payment_id, holder_id = holder_id, amount = amount, due_date = Sys.Date())
      payments <<- c(payments, list(payment))
    },
    
    schedule_payment = function(payment_id, holder_id, amount, due_date) {
      payment <- Payment$new(payment_id = payment_id, holder_id = holder_id, amount = amount, due_date = as.Date(due_date))
      payments <<- c(payments, list(payment))
    },
    
    process_payment = function(payment_id, date_paid = Sys.Date()) {
      for (payment in payments) {
        if (payment$payment_id == payment_id) {
          payment$mark_paid(date_paid)
          return()
        }
      }
      cat("Payment not found.\n")
    },
    
    send_payment_reminders = function() {
      cat("\n--- Payment Reminders ---\n")
      for (payment in payments) {
        if (payment$status == "Pending") {
          days_left <- as.integer(payment$due_date - Sys.Date())
          if (days_left <= 3) {
            cat(sprintf("Reminder: Payment %s for Holder %s is due in %d day(s).\n",
                        payment$payment_id, payment$holder_id, days_left))
          }
        }
      }
    },
    
    get_policyholder_info = function(holder_id) {
      if (!is.null(policyholders[[holder_id]])) {
        policyholders[[holder_id]]$show()
      } else {
        cat("Policyholder not found.\n")
      }
    },
    
    get_all_payments = function() {
      for (payment in payments) {
        payment$show()
      }
    }
  )
)
# Initialize the system
pms <- PolicyManagementSystem$new()

# Add policyholders
pms$add_policyholder("PH001", "Lia", "lia@example.com")
pms$add_policyholder("PH002", "Kinta", "kinta@example.com")
pms$add_policyholder("PH003", "Reza", "reza@example.com")

# Register products
pms$register_product("P001", "Health Insurance", 1000, "Medical")
pms$register_product("P002", "Life Insurance", 2000, "Life")

# Assign policies
pms$assign_policy("PH001", "P002")
pms$assign_policy("PH002", "P001")
pms$assign_policy("PH003", "P002")

# Schedule payments
pms$schedule_payment("PAY001", "PH001", 500, "2025-07-25")
pms$schedule_payment("PAY002", "PH002", 1000, "2025-07-20")
pms$schedule_payment("PAY003", "PH003", 2000, "2025-07-22")

# Show all assigned policies
pms$show_all_assigned_policies()

# Send payment reminders
pms$send_payment_reminders()

# Process a payment
pms$process_payment("PAY001")


# Display policyholder info
pms$get_policyholder_info("PH001")
pms$get_policyholder_info("PH002")
pms$get_policyholder_info("PH003")

# Show all payments
pms$get_all_payments()

